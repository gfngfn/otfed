

let assert_equal ~pp ~pp_error ~message:msg expected res =
  let ok_testable = Alcotest.of_pp pp in
  let error_testable = Alcotest.of_pp pp_error in
  let result_testable = Alcotest.result ok_testable error_testable in
  Alcotest.check result_testable msg (Ok(expected)) res


let get_or_fail ~pp_error k res =
  match res with
  | Ok(v)    -> k v
  | Error(e) -> Alcotest.failf "%a" pp_error e


let pp_sep ppf () =
  Format.fprintf ppf ", "


let pp_xxd ppf s =
  let pp_single ppf ch =
    Format.fprintf ppf "%02x" (Char.code ch)
  in
  let chars = Core_kernel.String.to_list_rev s |> List.rev in
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
