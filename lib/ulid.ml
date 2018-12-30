(*
  ULID is [48 bits timestamp (6 bytes)] + [80 bits random (10 bytes)]
  encoded in Crockford's base32
*)
(* Crockford's Base32 *)
let encoding = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F';'G';'H';'J';'K';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z';|];;
let encoding_len = Array.length encoding;;
let time_len = 10;;
Random.self_init ();;

let random_char () =
  let idx = Random.int encoding_len in
    Array.get encoding idx

let m = 0 mod 1;;

let encode_time now len =
  let t = ref now in
  let str = Buffer.create len in
  for _ = len downto 1 do
    let m = !t mod encoding_len in
    Buffer.add_char str (Array.get encoding m);
    t := (!t - m) / encoding_len;
  done;
  let s = (Buffer.contents str) in
  String.init len (fun i -> s.[len - 1 - i])

let increment_base_32 str = str