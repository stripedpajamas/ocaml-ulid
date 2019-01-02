val encoding : char array
val encoding_len : int
val time_len : int
val random_len : int
val get_nocrypto_rng : unit -> int -> int
val random_char : (int -> int) -> unit -> char
val find_in_array : 'a array -> 'a -> int -> int
val encode_time : int -> int -> string
val encode_random : int -> (int -> int) -> string
val replace_char_at : bytes -> int -> char -> bytes
val increment_base_32 : string -> string
val get_now : unit -> int

val ulid : ?seed_time:int -> unit -> string
val ulid_factory : ?prng:(int -> int) -> unit -> (?seed_time:int -> unit -> string)
val monotonic_factory : ?prng:(int -> int) -> unit -> (?seed_time:int -> unit -> string)
