(* these are the Basic Encoding Rules, standardized by the ITU-T in
   X.690 all comments containing "sec. x.x.x.x" are section numbers
   referring to sections in x.690

   Copyright (C) 2004 Eric Stokes, Matthew Backes, and The
   California State University at Northridge

   This library is free software; you can redistribute it and/or               
   modify it under the terms of the GNU Lesser General Public                  
   License as published by the Free Software Foundation; either                
   version 2.1 of the License, or (at your option) any later version.          
   
   This library is distributed in the hope that it will be useful,             
   but WITHOUT ANY WARRANTY; without even the implied warranty of              
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           
   Lesser General Public License for more details.                             
   
   You should have received a copy of the GNU Lesser General Public            
   License along with this library; if not, write to the Free Software         
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   
*)

exception Decoding_error of string
exception Encoding_error of string

(* our sole interface with the data is to read and write a byte.
   the user of the encodeing functions herin will pass a function
   of the type readbyte, or writebyte to us when the encoding function
   is called. We will use that function to get or set raw data *)
type readbyte = ?peek:bool -> unit -> char
type writebyte = (char -> unit)

(* note on syntax. In this program I use some somewhat little used,
   but very useful syntatic notations for numbers in Ocaml.
   eg. 0b11000000, the 0b indicates a binary number, everything after
   it is the number.  eg. 0b1100_0000, the _ has no meaning, it is
   just a seperator, however, seperating the nibbles in this way makes
   binary numbers much more readable *)
    
(* X.690 sec. 8.1.1 structure of an encoding *)
type ber_class = Universal
		 | Application
		 | Context_specific
		 | Private

type ber_length = Definite of int
		  | Indefinite

(* all the meta info about a ber value *)
type ber_val_header = {ber_class: ber_class;
		       ber_primitive: bool;
		       ber_tag: int;
		       ber_length: ber_length}

(* this type is a draft, and is not yet used,
   it will likely expand (and get more precise) 
   in the future. However the basic idea will be that
   the generalized decoder will return this type *)
type berval = Boolean of bool
	      | Int of Int32.t
	      | Bitstring of string
	      | Octetstring of string
	      | Null
	      | Oid of string
	      | Odt of string
	      | Real of float
	      | Enum of int32
	      | Utf8string of string
	      | RelativeOid of string
	      | Sequence of berval list
	      | Set of berval list
	      | Charstring of string
	      | Time of string

(* return a readbyte implementation which uses another readbyte, but
   allows setting a read boundry. Useful for constructing views of the
   octet stream which end at the end of a ber structure. This is
   essential for reading certian structures because lenght is only
   encoded in the toplevel in order to save space. *)
let readbyte_of_readbyte limit (rb:readbyte) = 
  let peek_counter = ref 1 
  and byte_counter = ref 0 in
  let f ?(peek=false) () =
    if not peek then
      if !byte_counter < limit then
	(peek_counter := 1;
	 byte_counter := !byte_counter + 1;
	 rb ())
      else raise Stream.Failure
    else if !peek_counter < limit then
      (peek_counter := !peek_counter + 1;
       rb ~peek:true ())
    else raise Stream.Failure
  in
    f

(* return a readbyte implementation which works using a string *)
let readbyte_of_string octets =
  let strm = Stream.of_string octets in
  let peek_counter = ref 1 in
  let limit = ref 0 in
  let f ?(peek=false) () =
    let rec last lst = (* thank god this is almost never used. puke O(n) ness puke *)
      match lst with
	  h :: [] -> h
	| h :: t -> last t
	| [] -> failwith "readbyte bug in \"last\" function!"
    in
      if not peek then
	(peek_counter := 1; (* reset the peek counter when we really read a byte *)
	 Stream.next strm)
      else	
	let elts = (Stream.npeek !peek_counter strm) in
	  if List.length elts = !peek_counter then
	    (peek_counter := !peek_counter + 1;
	     last elts)
	  else raise Stream.Failure (* if there are not enough elements in the stream, fail *)
  in
    f

let decode_ber_length ?(peek=false) (readbyte:readbyte) = (* sec. 8.1.3.3, the definite length form *)
  let octet = int_of_char (readbyte ~peek:peek ()) in
    if octet = 0b1111_1111 then
      (* sec/ 8.1.3.5c *)
      raise (Decoding_error "illegal initial length octet")
    else if octet = 0b1000_0000 then
      (* sec. 8.1.3.6 indefinite form *)
      Indefinite
    else if octet land 0b1000_0000 = 0b0000_0000 then
      (* sec. 8.1.3.4, definite length, short form *)
      Definite (octet land 0b0111_1111)
    else
      (* sec. 8.1.3.5, definite length, long form *)
      let rec decode_multioctet_length (readbyte:readbyte) numoctets remainingoctets value =
	if numoctets > 4 then raise (Decoding_error "length cannot be represented");
	if remainingoctets = 0 then Definite value
	else
	  let octet = int_of_char (readbyte ~peek:peek ()) in
	    if ((numoctets = 4) && (remainingoctets = 4) &&
		(octet land 0b1000_0000 = 0b1000_0000)) (* we have only 31 bits *)
	    then
	      raise (Decoding_error "length cannot be represented")
	    else
	      decode_multioctet_length readbyte numoctets (remainingoctets - 1) 
		(value + (octet lsl ((numoctets - (numoctets - remainingoctets) - 1) * 8)))
      in
      let numoctets = octet land 0b0111_1111 in
	decode_multioctet_length readbyte numoctets numoctets 0

let decode_ber_header ?(peek=false) (readbyte:readbyte) =
  let leading_octet = int_of_char (readbyte ~peek:peek ()) in
  let ber_tag = (* sec. 8.1.2.2c *)
    if leading_octet land 0b0001_1111 = 0b0001_1111 then
      (* sec. 8.1.2.4 multi octet tag encoding *)
      let rec decode_multioctet_tag (readbyte:readbyte) tag_value =
	let octet = int_of_char (readbyte ~peek:peek ()) in
	  if octet land 0b1000_0000 = 0b0000_0000 then tag_value + (octet land 0b0111_1111)
	  else decode_multioctet_tag readbyte (tag_value + (octet land 0b0111_1111))
      in
	decode_multioctet_tag readbyte 0
    else
      (* sec. 8.1.2.2 single octet tag encoding *)
      leading_octet land 0b0001_1111
  in
  let ber_length = decode_ber_length ~peek:peek readbyte in
    {ber_class = (* sec. 8.1.2.2a table 1 *)
       (match leading_octet land 0b1100_0000 with
	    0b0000_0000 -> Universal
	  | 0b0100_0000 -> Application
	  | 0b1000_0000 -> Context_specific
	  | 0b1100_0000 -> Private
	  | _ -> raise (Decoding_error "ber_class, decoder bug"));
     ber_primitive = (* sec. 8.1.2.5 *)
       (match leading_octet land 0b0100_0000 with
	    0b0100_0000 -> false (* value is constructed *)
	  | 0b0000_0000 -> true
	  | _ -> raise (Decoding_error "ber_primitive, decoder bug")); (* value is primative *)
     ber_tag = ber_tag;
     ber_length = ber_length}

let encode_ber_header {ber_class=cls;ber_primitive=pri;ber_tag=tag;ber_length=len} =
  let buf = Buffer.create 3 in
  let rec encode_multioctet_tag tag buf =
    if tag > 127 then 
      (Buffer.add_char buf (char_of_int 255);
       encode_multioctet_tag (tag - 127) buf)
    else
      Buffer.add_char buf (char_of_int tag)
  in
  let rec long_form_length len buf = (* sec 8.1.3.5 encode the length in up to 1 + 4 octets *)
    if len < 255 then (* fits in 8 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0001); (* long form with one octet *)
       Buffer.add_char buf (char_of_int len))
    else if len < 65535 then (* fits in 16 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0010); (* long form with two octets *)
       Buffer.add_char buf (char_of_int ((len land 0b11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int (len land 0b00000000_11111111)))
    else if len < 16777215 then (* fits in 24 bits? *)
      (Buffer.add_char buf (char_of_int 0b1000_0011); (* long form in three octets *)
       Buffer.add_char buf (char_of_int ((len land 0b11111111_00000000_00000000) lsr 16));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int (len land 0b00000000_00000000_11111111)))
    else (* can't currently encode anything bigger than 31 bits *)
      (Buffer.add_char buf (char_of_int 0b1000_0100);
       Buffer.add_char buf (char_of_int ((len land 0b00111111_00000000_00000000_00000000) lsr 24));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_11111111_00000000_00000000) lsr 16));
       Buffer.add_char buf (char_of_int ((len land 0b00000000_00000000_11111111_00000000) lsr 8));
       Buffer.add_char buf (char_of_int  (len land 0b00000000_00000000_00000000_11111111)));
  in
    Buffer.add_char buf (* deal with the header *)
      (char_of_int
	 ((match cls with
	       Universal -> 0b0000_0000
	     | Application -> 0b0100_0000
	     | Context_specific -> 0b1000_0000
	     | Private -> 0b1100_0000) lor
	  (if pri then 0b0000_0000 else 0b0010_0000) lor
	  (if tag > 31 then 0b0001_1111 else tag)));
    if tag > 31 then encode_multioctet_tag tag buf;
    (match len with (* deal with the length *)
	 Definite len ->
	   if len < 127 then Buffer.add_char buf (char_of_int len)
	   else long_form_length len buf;
       | Indefinite -> raise (Encoding_error "indefinite length encoding not implemented"));
    Buffer.contents buf

let read_contents ?(peek=false) (readbyte:readbyte) len =
  let rec readnbytes (readbyte:readbyte) buf n c =
      if c < n then
	(Buffer.add_char buf (readbyte ~peek:peek ());readnbytes readbyte buf n (c + 1))
      else Buffer.contents buf
  in
  let rec readuntileoc (readbyte:readbyte) buf =
    let octet1 = readbyte ~peek:peek () in
      if (int_of_char octet1) = 0b0000_0000 then
	let octet2 = readbyte ~peek:peek () in 
	  if (int_of_char octet2) = 0b0000_0000 then
	    Buffer.contents buf
	  else
	    (Buffer.add_char buf octet1;Buffer.add_char buf octet2;
	     readuntileoc readbyte buf)	    
      else 
	(Buffer.add_char buf octet1;readuntileoc readbyte buf)
  in
    match len with
	Definite n -> readnbytes readbyte (Buffer.create (n + 1)) n 0
      | Indefinite -> readuntileoc readbyte (Buffer.create 5)

let decode_ber_end_of_contents ?(peek=false) (readbyte:readbyte) =
  if not (((int_of_char (readbyte ~peek:peek ())) = 0) && 
	  (int_of_char (readbyte ~peek:peek ())) = 0) then 
    raise (Decoding_error "missing end of contents octets")

(* sec. 8.2 *)
let decode_ber_bool ?(peek=false) ?(cls=Universal) ?(tag=1) ?(contents=None) 
  (readbyte:readbyte) = 
  let decode_ber_bool' contents = 
    if (int_of_char contents.[0]) = 0 then false else true 
  in
    match contents with
	None ->
	  (match decode_ber_header ~peek:peek readbyte with
	       {ber_class=c;ber_tag=t;ber_length=bool_length} when c=cls && t=tag ->
		 decode_ber_bool' (read_contents ~peek:peek readbyte bool_length)
	     | _ -> raise (Decoding_error "expected bool"))
      | Some contents -> decode_ber_bool' contents

let encode_ber_bool ?(cls=Universal) ?(tag=1) value = 
  let buf = Buffer.create 3 in
    Buffer.add_string buf 
      (encode_ber_header 
	 {ber_class=cls;ber_primitive=true;ber_tag=tag;ber_length=Definite 1});
    Buffer.add_char buf
      (if value then char_of_int 1
       else char_of_int 0);
    Buffer.contents buf

(* sec 8.3 *)
let decode_ber_int32 ?(peek=false) ?(cls=Universal) ?(tag=2) ?(contents=None)
  (readbyte:readbyte) =
  let decode_ber_int32' contents =
    let length = String.length contents in
      if length > 5 then
	raise (Decoding_error "integer overflow, use bigger decode function?")
      else if length > 0 then
	let c i = Int32.of_int (int_of_char i) in
	let rec convert octets l i v = (* test more, << 32 is undef in manual, but seems to work *)
	  if i <= l then
	    convert octets l (i + 1) 
	      (Int32.add v (Int32.shift_left (c octets.[i]) (8 * (l - i))))
	  else v
	in
	  convert contents (length - 1) 0 0l
      else raise (Decoding_error "integer, no contents octets") (* sec 8.3.1 *)
  in
    match contents with
	None -> (* we have not yet read the header, and unpacked the contents *)
	  (match decode_ber_header ~peek:peek readbyte with
	       {ber_class=c;ber_tag=t;ber_length=int_length} when c=cls && t=tag ->
		 decode_ber_int32' (read_contents ~peek:peek readbyte int_length)
	     | _ -> raise (Decoding_error "expected int"))
      | Some contents -> decode_ber_int32' contents (* we already have the contents *)

let encode_ber_int32 ?(cls=Universal) ?(tag=2) value =  
  let to_char i = char_of_int (Int32.to_int i) in
  let buf = Buffer.create 4 in
    if value < 0b11111111l then (* fits in 8 bits? *)
      Buffer.add_char buf (to_char value)
    else if value < 0b11111111_11111111l then (* fits in 16 bits? *)
      (Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right
	       (Int32.logand value 0b11111111_00000000l)
	       8));
       Buffer.add_char buf 
	 (to_char (Int32.logand value 0b00000000_11111111l)))
    else if value < 0b11111111_11111111_11111111l then (* fits in 24 bits? *)
      (Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right 
	       (Int32.logand value 0b11111111_00000000_00000000l)
	       16));
       Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right 
	       (Int32.logand value 0b00000000_11111111_00000000l)
	       8));
       Buffer.add_char buf 
	 (to_char (Int32.logand value 0b00000000_00000000_11111111l)))
    else
      (Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right
	       (Int32.logand value 0b01111111_00000000_00000000_00000000l) 
	       24));
       Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right
	       (Int32.logand value 0b00000000_11111111_00000000_00000000l) 
	       16));
       Buffer.add_char buf 
	 (to_char 
	    (Int32.shift_right
	       (Int32.logand value 0b00000000_00000000_11111111_00000000l)
	       8));
       Buffer.add_char buf 
	 (to_char
	    (Int32.logand value 0b00000000_00000000_00000000_11111111l)));
    let buf1 = Buffer.create 5 in
      Buffer.add_string buf1
	(encode_ber_header
	   {ber_class=cls;
	    ber_tag=tag;
	    ber_primitive=true;
	    ber_length=Definite (Buffer.length buf)});
      Buffer.add_buffer buf1 buf;
      Buffer.contents buf1

(* sec. 8.4 *)
let decode_ber_enum ?(peek=false) ?(cls=Universal) ?(tag=10) ?(contents=None) 
  (readbyte:readbyte) = 
  decode_ber_int32 ~peek:peek ~cls:cls ~tag:tag ~contents:contents readbyte

let encode_ber_enum ?(cls=Universal) ?(tag=10) value = 
  encode_ber_int32 ~cls:cls ~tag:tag value

(* sec 8.7 *)
let decode_ber_octetstring ?(peek=false) ?(cls=Universal) ?(tag=4) ?(contents=None) 
  (readbyte:readbyte) = 
  let decode_ber_octetstring' contents = contents in
    match contents with
	None -> (* have not yet read the header, or unpacked the contents *)
	  (match decode_ber_header readbyte with
	       {ber_class=c;ber_tag=t;ber_length=octetstring_length} when c=cls && t=tag ->
		 decode_ber_octetstring' (read_contents ~peek:peek readbyte octetstring_length)
	     | _ -> raise (Decoding_error "expected octetstring"))
      | Some contents -> decode_ber_octetstring' contents (* already read header *)

let encode_ber_octetstring ?(cls=Universal) ?(tag=4) string = 
  let len = String.length string in
  let buf = Buffer.create (len + 3) in
    Buffer.add_string buf
      (encode_ber_header
	 {ber_class=cls;
	  ber_tag=tag;
	  ber_primitive=true;
	  ber_length=Definite len});
    Buffer.add_string buf string;
    Buffer.contents buf

let encode_ber_null ?(cls=Universal) ?(tag=5) () =
  encode_ber_header {ber_class=cls; 
                     ber_tag=tag; 
                     ber_primitive=true;
                     ber_length=Definite 0}

let decode_ber_null ?(peek=false) ?(cls=Universal) ?(tag=5) ?(contents=None)
  (readbyte:readbyte) = 
  let decode_ber_null' contents = () in
    match contents with
        None ->
          (match decode_ber_header ~peek:peek readbyte with
               {ber_class=c; ber_tag=t; ber_length=l} 
                 when c=cls && t=tag && l=Definite 0 ->
                   decode_ber_null' None
             | _ -> raise (Decoding_error "expected null"))
      | Some contents -> decode_ber_null' contents

let rec encode_berval_list ?(buf=Buffer.create 50) efun lst =
  match lst with
      hd :: [] -> 
	Buffer.add_string buf (efun hd);
	Buffer.contents buf
    | hd :: tl ->
	(encode_berval_list
	   ~buf:(Buffer.add_string buf (efun hd);buf) efun tl)
    | [] -> Buffer.contents buf

let rec decode_berval_list ?(lst=[]) dfun (readbyte:readbyte) =
  try decode_berval_list ~lst:((dfun readbyte) :: lst) dfun readbyte
  with Stream.Failure -> lst

