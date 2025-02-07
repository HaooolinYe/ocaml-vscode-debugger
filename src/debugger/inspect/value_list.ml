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

open Value_basic

let nil_value =
  object
    inherit value

    method to_short_string = "[]"
  end

class list_value ~scene ~typenv ~obj ~element_type () =
  object
    inherit value

    method to_short_string =
      let single_str = "[e]" in 
      let many_str = "‹hd› :: ‹tl›" in 
      let choose_name cond = if cond then many_str else single_str in 
      begin
        if Scene.is_block obj then 
          match obj with
          | Scene.Local v  -> Obj.is_block (Obj.field v 1) |> choose_name
          | Remote rv -> "list"
            (* 
            "list remote-rv: " ^ 
            string_of_bool (Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 0))
            ^ 
            string_of_bool (Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 1))
            *)
            (* Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 1) |> choose_name *)
        else 
          "list_empty_buggy"

        (* Lwt_main.run
          (let%lwt obj' = Scene.get_field scene obj 1 in
            if Scene.is_block obj' then
              Lwt.return "‹hd› :: ‹tl›"
            else 
              Lwt.return "[e]") *)
      end


    method! num_named = 2

    method! list_named =
      let rec unroll_obj cur_obj k = 
        let index_str = "‹" ^ (string_of_int k) ^ "›" in
        let%lwt hd =
          let%lwt obj' = Scene.get_field scene cur_obj 0 in adopt scene typenv obj' element_type
        in
      
        let%lwt obj' = Scene.get_field scene cur_obj 1 in
        
        if Scene.is_block obj' then
          let%lwt res = (unroll_obj obj' (k+1)) in 
          Lwt.return ((index_str,hd) :: res)
        else
          Lwt.return [(index_str, hd)]
      in 

      unroll_obj obj 0
      
      (*
      let%lwt hd =
        let%lwt obj' = Scene.get_field scene obj 0 in
        adopt scene typenv obj' element_type
      in
      
      let%lwt obj' = Scene.get_field scene obj 1 
      in

      if Scene.is_block obj' then
        begin
          let%lwt tl =
          Lwt.return (new list_value ~scene ~typenv ~obj:obj' ~element_type ())
          in Lwt.return [ ("‹hd›", hd); ("‹tl›", tl); ("nil_test", nil_value); ("nil_test", nil_value) ]
        end
      else 
        Lwt.return [ ("‹hd›", hd)]
          (* Lwt.return (nil_value) *)
      (* Lwt.return [ ("‹hd›", hd); ("‹tl›", tl) ] *)
      *)
  end

let adopter scene typenv obj typ =
  match (Ctype.repr typ).desc with
  | Tconstr (_, [ element_type ], _)
    when Typenv.type_matches typenv (Predef.type_list element_type) typ ->
      if Scene.is_block obj then
        Lwt.return (Some (new list_value ~scene ~typenv ~obj ~element_type ()))
      else Lwt.return (Some nil_value)
  | _ -> Lwt.return None
