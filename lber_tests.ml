open Lber
open Unix

let encode_decode i = 
  let encode_start = gettimeofday () in
  let e_i32 =  encode_ber_int32 i in
  let encode_finish = gettimeofday () in
  let rb = readbyte_of_string e_i32 in
  let decode_start = gettimeofday () in
  let decoded_value = decode_ber_int32 rb in
  let decode_finish = gettimeofday () in
    (encode_finish -. encode_start,
     decode_finish -. decode_start,
     decoded_value)

let rec test_positive_encode_decode 
    ?(encode_time_total=0.0) 
    ?(decode_time_total=0.0) 
    ?(finishvalue=Int32.max_int) i = 
  if i < finishvalue then
    let (encode_time_taken, decode_time_taken, result) = encode_decode i in
      if  result <> i then
	failwith ("I encode: " ^ (Int32.to_string i) ^ 
		    " and I get: " ^ (Int32.to_string result))
      else 
	test_positive_encode_decode 
	  ~encode_time_total:(encode_time_taken +. encode_time_total)
	  ~decode_time_total:(decode_time_taken +. decode_time_total)
	  ~finishvalue:finishvalue
	  (Int32.succ i)
  else
    (encode_time_total, decode_time_total)
