
type 'a set = 'a list

module ResultMonad : sig
  val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val return : 'a -> ('a, 'e) result
  val err : 'e -> ('a, 'e) result
end = struct

  let ( >>= ) x f =
    match x with
    | Ok(v)           -> f v
    | Error(_) as err -> err


  let return v =
    Ok(v)


  let err e =
    Error(e)

end

type offset = int
