(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ground
open Value_basic

class virtual struct_value ~scene ~typenv ~obj ~pos ~unboxed ~members =
  object
    inherit value

    method! num_named = if Scene.is_block obj then List.length members else 0

    method! list_named =
      if unboxed then
        let name, typ = List.hd members in
        let%lwt value = adopt scene typenv obj typ in
        Lwt.return [ (name, value) ]
      else
        members
        |> Lwt_list.mapi_s (fun i (name, typ) ->
               let%lwt obj' = Scene.get_field scene obj (pos + i) in
               let%lwt value = adopt scene typenv obj' typ in
               Lwt.return (name, value))
  end

class tuple_value ~scene ~typenv ~obj ?(pos = 0) ?(unboxed = false) ~members ()
  =
  let members =
    members
    |> List.mapi (fun i typ -> ("‹arg" ^ string_of_int (i + 1) ^ "›", typ))
  in
  object (self)
    inherit struct_value ~scene ~typenv ~obj ~pos ~unboxed ~members

    method to_short_string =
      let num_named = self#num_named in
      let range = List.init num_named (fun x -> x + 1) in 
      let r = 
      range |> List.map (fun x -> Printf.sprintf "‹arg%d›" x) |> String.concat ", " 
      in "(" ^ r ^ ")"
(* 
      if num_named = 0 then "()"
      else if num_named = 1 then "‹1›"
      else if num_named = 2 then  "(‹1›, ‹2›)" (* DBG: xujie "(‹1›, ‹2›)" *)
      else if num_named = 3 then "(‹1›, ‹2›, ‹3›)"
      else "(‹1›, ‹2›, ‹3›, …)"
*)      
  end

class record_value ~scene ~typenv ~obj ?(pos = 0) ?(unboxed = false) ~members ()
  =
  object
    inherit struct_value ~scene ~typenv ~obj ~pos ~unboxed ~members

    method to_short_string = 
      (* DBG: xujie "{…}" *)
      let k = 4 in 
      if (List.length members) <= k then 
        let r = members |> List.map (fun (x,_) -> x) |> String.concat "," in 
        "{" ^ r ^ "}"
      else
        begin
          let first_k = List.init k (fun x -> x) |> List.map (List.nth members) in 
          let r = List.map (fun (x,_) -> x) first_k |> String.concat ", " in 
          "{" ^ r ^ ", …" ^ (string_of_int (List.length members)) ^ " members}"

          (* "<record> " ^ (string_of_int (List.length members)) ^ " members"  *)
        end
  end

(* 
 * variant value captures user-defined constructors 
 * more accurately, any constructors that do not have a special `X_value class`  
*)  
class variant_value ~tag ?payload ?(embed = false) () =
  object
    inherit value

    method to_short_string =
      match payload with
      | Some payload ->
          if embed then tag ^ payload#to_short_string
          else tag ^ " ‹1›"
      | None -> tag

    method! num_named =
      match payload with
      | Some payload -> if embed then payload#num_named else 1
      | None -> 0

    method! list_named =
      match payload with
      | Some payload ->
          if embed then payload#list_named
          else Lwt.return [ ("‹1›", payload) ]
      | None -> Lwt.return []
  end

let adopter scene typenv obj typ =
  let record typ type_args =
    let labels, rep =
      match typ.Types.type_kind with
      | Type_record (labels, rep) -> (labels, rep)
      | _ -> assert false
    in
    let type_params = typ.type_params in
    let unboxed = match rep with Record_unboxed _ -> true | _ -> false in
    let pos = match rep with Record_extension _ -> 1 | _ -> 0 in
    let apply_type ty =
      try Typenv.type_apply typenv type_params ty type_args
      with Ctype.Cannot_apply -> ty
    in
    let members =
      labels
      |> List.map (fun lbl ->
             let id = lbl.Types.ld_id |> Ident.name in
             let typ = apply_type lbl.ld_type in
             (id, typ))
    in
    Lwt.return
      (Some (new record_value ~scene ~typenv ~obj ~pos ~unboxed ~members ()))
  in
  let poly_variant row =
    let row = Btype.row_repr row in
    if Scene.is_block obj then
      let%lwt tag = Scene.get_field scene obj 0 in
      let%lwt tag = Scene.marshal_obj scene tag in
      let rec find = function
        | (l, f) :: fields ->
            if Btype.hash_variant l = tag then
              match Btype.row_field_repr f with
              | Rpresent (Some ty) | Reither (_, [ ty ], _, _) -> Some (l, ty)
              | _ -> find fields
            else find fields
        | [] -> None
      in
      match find row.row_fields with
      | Some (l, typ') ->
          let%lwt obj' = Scene.get_field scene obj 1 in
          let%lwt payload = adopt scene typenv obj' typ' in
          Lwt.return (Some (new variant_value ~tag:("`" ^ l) ~payload ()))
      | None -> Lwt.return None
    else
      let%lwt tag = Scene.marshal_obj scene obj in
      let rec find = function
        | (l, _) :: fields ->
            if Btype.hash_variant l = tag then Some l else find fields
        | [] -> None
      in
      match find row.row_fields with
      | Some l -> Lwt.return (Some (new variant_value ~tag:("`" ^ l) ()))
      | None -> Lwt.return None
  in
  let extensiable_variant () =
    let%lwt tag = Scene.get_tag scene obj in
    let%lwt slot =
      if tag <> 0 then Lwt.return obj else Scene.get_field scene obj 0
    in
    let%lwt id = Scene.get_field scene slot 0 in
    let%lwt tag = Scene.marshal_obj scene id in
    let longid = Parse.longident (Lexing.from_string tag) in
    let%lwt payload =
      match typenv |> Typenv.find_constructor_by_name longid with
      | cstr_decl ->
          if cstr_decl.cstr_args = [] then Lwt.return None
          else
            Lwt.return
              (Some
                 (new tuple_value
                    ~scene ~typenv ~obj ~pos:1
                    ~unboxed:(cstr_decl.cstr_inlined |> Option.is_some)
                    ~members:cstr_decl.cstr_args ()))
      | exception Not_found -> Lwt.return None
    in
    Lwt.return (Some (new variant_value ~tag ?payload ~embed:true ()))
  in
  let variant typ type_args =
    let constr_list =
      match typ.Types.type_kind with
      | Type_variant constr_list -> constr_list
      | _ -> assert false
    in
    let unboxed = typ.type_unboxed.unboxed in
    let type_params = typ.type_params in
    let%lwt tag =
      if unboxed then Lwt.return Types.Cstr_unboxed
      else if not (Scene.is_block obj) then
        let%lwt tag = Scene.marshal_obj scene obj in
        Lwt.return (Types.Cstr_constant tag)
      else
        let%lwt tag = Scene.get_tag scene obj in
        Lwt.return (Types.Cstr_block tag)
    in
    let apply_type ty =
      try Typenv.type_apply typenv type_params ty type_args
      with Ctype.Cannot_apply -> ty
    in
    match Datarepr.find_constr_by_tag tag constr_list with
    | exception Datarepr.Constr_not_found -> Lwt.return None
    | constr ->
        let%lwt payload =
          match constr.cd_args with
          | Types.Cstr_tuple [] -> Lwt.return None
          | Types.Cstr_tuple types ->
              let members = types |> List.map apply_type in
              Lwt.return
                (Some
                   (new tuple_value
                      ~scene ~typenv ~obj ~pos:0 ~unboxed ~members ()))
          | Cstr_record labels ->
              let members =
                labels
                |> List.map (fun lbl ->
                       let id = lbl.Types.ld_id |> Ident.name in
                       let typ = apply_type lbl.ld_type in
                       (id, typ))
              in
              Lwt.return
                (Some
                   (new record_value
                      ~scene ~typenv ~obj ~pos:0 ~unboxed ~members ()))
        in
        let tag = (Ident.name constr.cd_id)  in
        Lwt.return (Some (new variant_value ~tag ?payload ~embed:true ()))
  in
  match (Ctype.repr typ).desc with
  | Ttuple tys ->
      Lwt.return (Some (new tuple_value ~scene ~typenv ~obj ~members:tys ()))
  | Tconstr (path, type_args, _) -> (
      match typenv |> Typenv.find_type path with
      | exception Not_found -> Lwt.return None
      | { type_kind = Type_record _; _ } as t -> record t type_args
      | { type_kind = Type_variant _; _ } as t -> variant t type_args
      | { type_kind = Type_open; _ } -> extensiable_variant ()
      | _ -> Lwt.return None )
  | Tvariant row -> poly_variant row
  | _ -> Lwt.return None
