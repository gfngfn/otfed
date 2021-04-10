
include module type of EncodeOperationCore

open Open

(** [e_8bits bs] writes a sequence [bs] of bits (of length less than or equal to 8)
    as an unsigned one-byte integer. *)
val e_8bits : bool list -> unit encoder

(** [e_16bits bs] writes a sequence [bs] of bits (of length less than or equal to 16)
    as an unsigned two-byte integer. *)
val e_16bits : bool list -> unit encoder

val e_f2dot14 : float -> unit encoder

(** [mapM enc xs] writes [xs] by using [enc] for each element of [xs]. *)
val mapM : ('a -> 'b encoder) -> 'a list -> ('b list) encoder

(** [e_list enc xs] is the same as [mapM enc xs >>= fun _ -> return ()]. *)
val e_list : ('a -> unit encoder) -> 'a list -> unit encoder

(** Adds null bytes if needed so that
    the next position is set to be long-aligned (i.e. multiples of 4). *)
val pad_to_long_aligned : unit encoder
