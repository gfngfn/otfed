
include module type of EncodeOperationCore

open Open

(** [e_8bits bs] writes a sequence [bs] of bits (of length less than or equal to 8)
    as an unsigned one-byte integer. *)
val e_8bits : bool list -> unit encoder

(** [e_16bits bs] writes a sequence [bs] of bits (of length less than or equal to 16)
    as an unsigned two-byte integer. *)
val e_16bits : bool list -> unit encoder

(** [e_list enc xs] writes [xs] by using [enc] for each element of [xs]. *)
val e_list : ('a -> unit encoder) -> 'a list -> unit encoder
