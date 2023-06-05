
open Otfed__Basic
module DecodeOperation = Otfed__DecodeOperation
module EncodeOperation = Otfed__EncodeOperation
module DecodeError = Otfed__DecodeError
module EncodeError = Otfed__EncodeError


let get_or_fail ~pp_error k res =
  match res with
  | Ok(v)    -> k v
  | Error(e) -> Alcotest.failf "%a" pp_error e


let pp_list pp_elem ppf elems =
  let pp_sep ppf () = Format.fprintf ppf ";@ " in
  Format.fprintf ppf "[%a]" (Format.pp_print_list ~pp_sep pp_elem) elems


let pp_xxd ppf s =
  let pp_single ppf ch =
    Format.fprintf ppf "%02x" (Char.code ch)
  in
  let chars = Base.String.to_list s in
  Format.fprintf ppf "%a" (Format.pp_print_list pp_single) chars


let string_of_uint16_scheme ns nopt =
  let buf = Buffer.create (List.length ns * 2 + 2) in
  ns |> List.iter (fun n ->
    let c1 = Char.chr (n lsr 8) in
    let c2 = Char.chr (n land 255) in
    Buffer.add_char buf c1;
    Buffer.add_char buf c2);
  begin
    match nopt with
    | None    -> ()
    | Some(n) -> Buffer.add_char buf (Char.chr n)
  end;
  Buffer.contents buf


let make_string_even ns =
  string_of_uint16_scheme ns None


let make_string_odd ns n =
  string_of_uint16_scheme ns (Some(n))


let run_decoder s d =
  DecodeOperation.run { data = s; max = String.length s } 0 d


let run_encoder enc =
  let open ResultMonad in
  enc |> EncodeOperation.run >>= fun (contents, _) ->
  return contents


let decoding testable_ok =
  Alcotest.(result testable_ok (of_pp DecodeError.pp))


let encoding =
  Alcotest.(result (of_pp pp_xxd) (of_pp EncodeError.pp))


let uchar =
  Alcotest.(of_pp pp_uchar)


let glyph_id =
  Alcotest.int


let utf8_to_utf16be s_utf8 =
  let buffer = Buffer.create (String.length s_utf8 * 4) in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(s_utf8)) in
  let encoder = Uutf.encoder `UTF_16BE (`Buffer(buffer)) in
  let rec loop () =
    match Uutf.decode decoder with
    | `Await | `Malformed(_) ->
        assert false

    | `End ->
        Uutf.encode encoder `End |> ignore

    | `Uchar(ch) ->
        Uutf.encode encoder (`Uchar(ch)) |> ignore;
        loop ()
  in
  loop ();
  Buffer.contents buffer
