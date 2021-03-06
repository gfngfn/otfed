
type 'a set = 'a list

module ImmutStack : sig
  type 'a t
  val empty : 'a t
  val size : 'a t -> int
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a t * 'a) option
  val pop_all : 'a t -> 'a list
end = struct
  type 'a t = 'a list

  let empty = []

  let size = List.length

  let push x stack = x :: stack

  let pop = function
    | []      -> None
    | x :: xs -> Some((xs, x))

  let pop_all = List.rev
end

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val chop_last : 'a t -> ('a t * 'a) option
end = struct
  type 'a t = 'a list

  let empty = []

  let extend acc x = x :: acc

  let append acc xs = List.rev_append xs acc

  let to_list = List.rev

  let is_empty = function
    | []     -> true
    | _ :: _ -> false

  let chop_last acc =
    match acc with
    | []      -> None
    | x :: xs -> Some((xs, x))
end

module ResultMonad = struct
  let ( >>= ) x f =
    match x with
    | Ok(v)           -> f v
    | Error(_) as err -> err

  let return v =
    Ok(v)

  let err e =
    Error(e)

  let mapM f xs =
    let res =
      xs |> List.fold_left (fun res x ->
        res >>= fun acc ->
        f x >>= fun y ->
        return @@ Alist.extend acc y
      ) (Ok(Alist.empty))
    in
    res >>= fun acc ->
    return @@ Alist.to_list acc
end

type offset = int
[@@deriving show { with_path = false }]

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
[@@deriving show {with_path = false}]

let ( +% ) = WideInt.add

let ( -% ) = WideInt.sub

let ( !% ) = WideInt.of_int

let ( !%% ) = WideInt.of_int64

let is_in_range ~lower:a ~upper:b x = (a <= x && x <= b)
