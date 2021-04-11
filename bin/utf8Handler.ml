
open Common


let to_uchar_list (s : string) : (Uchar.t list, error) result =
  let open ResultMonad in
  let decoder = Uutf.decoder ~encoding:`UTF_8 (`String(s)) in
  let rec aux (uchacc : Uchar.t Alist.t) =
    match Uutf.decode decoder with
    | `Await
    | `Malformed(_) ->
        err @@ InvalidUtf8String

    | `End ->
        return @@ Alist.to_list uchacc

    | `Uchar(uch) ->
        aux (Alist.extend uchacc uch)
  in
  aux Alist.empty
