
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
