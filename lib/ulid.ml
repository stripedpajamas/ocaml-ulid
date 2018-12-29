(*
  ULID is [48 bits timestamp (6 bytes)] + [80 bits random (10 bytes)]
  encoded in Crockford's base32
*)

let increment_base_32 str = str