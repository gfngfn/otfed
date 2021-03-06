
let string_of_uint16_list ns =
  let buf = Buffer.create (List.length ns * 2 + 2) in
  ns |> List.iter (fun n ->
    let c1 = Char.chr (n lsr 8) in
    let c2 = Char.chr (n land 255) in
    Buffer.add_char buf c1;
    Buffer.add_char buf c2);
  Buffer.contents buf
