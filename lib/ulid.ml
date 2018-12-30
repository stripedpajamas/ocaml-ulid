(*
  ULID is [48 bits timestamp] + [80 bits random]
  encoded in Crockford's base32
*)
(* Crockford's Base32 *)
let encoding = [|'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'A';'B';'C';'D';'E';'F';'G';'H';'J';'K';'M';'N';'P';'Q';'R';'S';'T';'V';'W';'X';'Y';'Z';|];;
let encoding_len = Array.length encoding;;
let time_len = 10;;
let random_len = 16;;
Random.self_init ();;

let random_char () =
  let idx = Random.int encoding_len in
    Array.get encoding idx

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

let encode_random len = 
  let str = Buffer.create len in
  for _ = len downto 1 do
    Buffer.add_char str (random_char ())
  done;
  let s = (Buffer.contents str) in
  String.init len (fun i -> s.[len - 1 - i])

let increment_base_32 str = str

let get_now () =
  int_of_float (1000. *. (Unix.gettimeofday ()))

let ulid ?(seed_time = (get_now ())) () =
  let s = Buffer.create (time_len + encoding_len) in
  Buffer.add_string s (encode_time seed_time time_len);
  Buffer.add_string s (encode_random random_len);
  Buffer.contents s
