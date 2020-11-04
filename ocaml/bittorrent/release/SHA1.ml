type t = bytes

let sha1 = Cryptokit.Hash.sha1 ()

let hash = Cryptokit.hash_string sha1

let to_bytes x = Bytes.copy x

let of_bytes x = Bytes.copy x

