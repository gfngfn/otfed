
type 'a set = 'a list

module ImmutStack : sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val size : 'a t -> int
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> ('a t * 'a) option
  val pop_all : 'a t -> 'a list
end

module Alist : sig
  type 'a t
  val empty : 'a t
  val extend : 'a t -> 'a -> 'a t
  val append : 'a t -> 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val is_empty : 'a t -> bool
  val chop_last : 'a t -> ('a t * 'a) option
end

module ResultMonad : sig
  val ( >>= ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val return : 'a -> ('a, 'e) result
  val err : 'e -> ('a, 'e) result
  val mapM : ('a -> ('b, 'e) result) -> 'a list -> ('b list, 'e) result
  val foldM : ('b -> 'a -> ('b, 'e) result) -> 'a list -> 'b -> ('b, 'e) result
end

type offset = int
[@@deriving show { with_path = false }]

module WideInt : sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val ( lor ) : t -> t -> t
  val ( land ) : t -> t -> t
  val ( mod ) : t -> t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_byte : char -> t
  val to_byte : t -> char
  val is_in_int32 : t -> bool
  val is_in_uint32 : t -> bool
  val is_in_int64 : t -> bool
  val is_neg : t -> bool
  val is_pos : t -> bool
end

type wint = WideInt.t
[@@deriving show { with_path = false }]

val ( +% ) : wint -> wint -> wint

val ( -% ) : wint -> wint -> wint

val ( /% ) : wint -> wint -> wint

val ( !% ) : int -> wint

val ( !%% ) : int64 -> wint

val is_in_range : lower:int -> upper:int -> int -> bool

val pp_uchar : Format.formatter -> Uchar.t -> unit

val pp_option : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val macintosh_glyph_name_list : string list
