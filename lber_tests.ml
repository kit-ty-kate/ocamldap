open Lber

let encode_decode i = 
  let e_i32 = encode_ber_int32 i in
  let rb = readbyte_of_string e_i32 in
    decode_ber_int32 rb

let rec test_positive_encode_decode i = 
  if i < Int32.max_int then
    let result = encode_decode i in
      if  result <> i then
	failwith ("I encode: " ^ (Int32.to_string i) ^ 
		    " and I get: " ^ (Int32.to_string result))
      else test_positive_encode_decode (Int32.succ i)
