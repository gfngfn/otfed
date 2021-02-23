
let print (common, _) =
  let open Otfed.ResultMonad in
  let _ =
    let tables = Otfed.Decode.tables common in
    tables |> List.iter (fun tag -> print_endline (Otfed.Value.Tag.to_string tag));
    return ()
  in
  ()


let _ =
  let s = Core_kernel.In_channel.read_all Sys.argv.(1) in
  let open Otfed.ResultMonad in
  Otfed.Decode.source_of_string s >>= function
  | Single(source) ->
      print source;
      return ()

  | Collection(sources) ->
      sources |> List.iter print;
      return ()
