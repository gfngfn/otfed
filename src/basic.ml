
type 'a set = 'a list

module ResultMonad = struct
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

module WideInt = struct
  type t = Int64.t

  let equal = Int64.equal

  let compare = Int64.compare

  let pp fmt iw = Format.fprintf fmt "%LX" iw

  let ( lsl ) = Int64.shift_left

  let ( lsr ) = Int64.shift_right

  let ( lor ) = Int64.logor

  let ( land ) = Int64.logand

  let ( mod ) = Int64.rem

  let add = Int64.add

  let sub = Int64.sub

  let of_int = Int64.of_int

  let to_int = Int64.to_int

  let of_int64 iw = iw

  let to_int64 iw = iw

  let of_byte ch = Int64.of_int (Char.code ch)

  let to_byte iw = Char.chr (Int64.to_int iw)  (* -- may raise 'Invalid_argument' -- *)

  let is_in_uint32 iw = Int64.zero <= iw && iw < 0x100000000L

  let is_in_int32 iw = -0x80000000L <= iw && iw < 0x80000000L

  let is_in_int64 iw = Int64.min_int <= iw && iw <= Int64.max_int

  let is_neg iw = iw < Int64.zero
end

type wint = WideInt.t

let ( +% ) = WideInt.add

let ( -% ) = WideInt.sub

let ( !% ) = WideInt.of_int

let ( !%% ) = WideInt.of_int64
