(* 
   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)


open Lber
open Ldap_types
open Ldap_protocol
open Unix

exception Server_error of string
exception Finished

type connection_id = int

type backendInfo = {
  bi_op_bind : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_unbind : (connection_id -> ldap_message -> unit) option;
  bi_op_search : (connection_id -> ldap_message -> (unit -> ldap_message)) option;
  bi_op_compare : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modify : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modrdn : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_add : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_delete : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_abandon : (connection_id -> ldap_message -> unit) option;
  bi_op_extended : (connection_id -> ldap_message -> ldap_message) option;
  bi_init : (unit -> unit) option;
  bi_close : (unit -> unit) option;
}

type msgid = int
type pending_operations = (unit -> unit) list

type server_info = {
  si_listening_socket: file_descr;
  si_client_sockets: (file_descr, connection_id * pending_operations * readbyte) Hashtbl.t;  
  si_backend: backendInfo;
  mutable si_current_connection_id: int;
}

let allocate_connection_id si =
  if si.si_current_connection_id < max_int then
    (si.si_current_connection_id <- si.si_current_connection_id + 1;
     si.si_current_connection_id)
  else
    (si.si_current_connection_id <- 1;1)

let send_message fd msg =
  let e_msg = encode_ldapmessage msg in
  let len = String.length e_msg in
  let written = ref 0 in
    try
      print_endline ("sending message id: " ^ (string_of_int msg.messageID));
      while !written < len
      do
	written := ((write fd e_msg 
		       !written (len - !written)) + 
		      !written)
      done
    with Unix_error (_, _, _) ->
      (try close fd with _ -> ());
      raise (Server_error "data cannot be written")

let keys h = Hashtbl.fold (fun k v l -> k :: l) h []

let init ?(port=389) bi =
  let s = 
    let s = socket PF_INET SOCK_STREAM 0 in
      setsockopt s SO_REUSEADDR true;
      bind s (ADDR_INET (inet_addr_any, port));
      listen s 500;
      s
  in
    (match bi.bi_init with
	 Some f -> f ()
       | None -> ());
    {si_listening_socket=s;
     si_client_sockets=Hashtbl.create 10;
     si_current_connection_id=0;
     si_backend=bi}

let shutdown si = 
  (match si.si_backend.bi_close with
       Some f -> f ()
     | None -> ());
  close si.si_listening_socket;
  List.iter (fun fd -> close fd) (keys si.si_client_sockets);
  Hashtbl.clear si.si_client_sockets

let dispatch_request bi conn_id rb fd =
  let not_imp msg op = 
    {messageID=msg.messageID;
     protocolOp=op;
     controls=None}
  in
  let not_implemented = {result_code=`OTHER;
			 matched_dn="";
			 error_message="Not Implemented";
			 ldap_referral=None}
  in
  let message = decode_ldapmessage rb in
    print_endline ("message id: " ^ (string_of_int message.messageID));
    match message with
	{protocolOp=Bind_request _} -> 
	  print_endline "bind request";
	  (match bi.bi_op_bind with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Bind_response
					      {bind_result=not_implemented;
					       bind_serverSaslCredentials=None}));
			  raise Finished))
      | {protocolOp=Unbind_request} ->
	  print_endline "unbind request";
	  (match bi.bi_op_unbind with
	       Some f -> (fun () -> f conn_id message;raise Finished)
	     | None -> (fun () -> raise Finished))
      | {protocolOp=Search_request _} ->
	  print_endline "search request";
	  (match bi.bi_op_search with
	       Some f -> 
		 let get_srch_result = f conn_id message in		   
		   (fun () -> send_message fd (get_srch_result ()))
	     | None -> (fun () -> send_message fd 
			  (not_imp message (Search_result_done not_implemented));
			  raise Finished))
      | {protocolOp=Modify_request _} ->
	  print_endline "modify request";
	  (match bi.bi_op_modify with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Modify_response not_implemented));
			  raise Finished))
      | {protocolOp=Add_request _} ->
	  print_endline "add request";
	  (match bi.bi_op_add with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Add_response not_implemented));
			  raise Finished))
      | {protocolOp=Delete_request _} ->
	  print_endline "delete request";
	  (match bi.bi_op_delete with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Delete_response not_implemented));
			  raise Finished))
      | {protocolOp=Modify_dn_request _} ->
	  print_endline "modify request";
	  (match bi.bi_op_modrdn with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Modify_dn_response not_implemented));
			  raise Finished))
      | {protocolOp=Compare_request _} ->
	  print_endline "compare request";
	  (match bi.bi_op_compare with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message (Compare_response not_implemented));
			  raise Finished))
      | {protocolOp=Abandon_request _} ->
	  print_endline "abandon request";
	  (match bi.bi_op_abandon with
	       Some f -> (fun () -> f conn_id message;raise Finished)
	     | None -> (fun () -> raise Finished))
      | {protocolOp=Extended_request _} ->
	  print_endline "extended request";
	  (match bi.bi_op_extended with
	       Some f -> (fun () -> send_message fd (f conn_id message);raise Finished)
	     | None -> (fun () -> send_message fd
			  (not_imp message 
			     (Extended_response 
				{ext_result=not_implemented;
				 ext_responseName=None;
				 ext_response=None}));
			  raise Finished))
      | _ -> raise (Server_error "invalid operation")

(* old rb_of_fd
  let rb_of_fd fd =
    let buf = String.create 1 in
    let in_ch = in_channel_of_descr fd in (* greatly improves performace *)
    let rec rb ?(peek=false) () = 
      let result = input in_ch buf 0 1 in
	if result = 1 then
	  buf.[0]
	else (close fd;raise (Server_error "socket error"))
    in
      rb
  in    
*)

let run si = 
  let pending_writes si = (* do we have data to write? *)
    Hashtbl.fold 
      (fun k (_, ops_pending, _) pending -> 
	 match ops_pending with 
	     [] -> pending
	   | _ -> k :: pending)
      si.si_client_sockets []
  in
    while true
    do
      let fds = keys si.si_client_sockets in
      let reading = ref []
      and writing = ref []
      and excond = ref [] in
      let (rd, wr, ex) = 
	print_endline "waiting for data to come in";
	select (si.si_listening_socket :: fds) 
	  (pending_writes si) (* nothing to write? don't bother *)
	  fds (-1.0) 
      in
	reading := rd;writing := wr;excond := ex;
	let process_read (fd:file_descr) =
	  if Hashtbl.mem si.si_client_sockets fd then
	    (* an existing client has requested a new operation *)	  
	    let (conn_id, pending_ops, rb) = Hashtbl.find si.si_client_sockets fd in
	      print_endline "processing read operation";
	      try
		Hashtbl.replace si.si_client_sockets fd
		  (conn_id, (dispatch_request si.si_backend conn_id rb fd) :: pending_ops, rb)
	      with Readbyte_error Transport_error ->
		print_endline "closing client connection due to read error";
		(match si.si_backend.bi_op_unbind with
		     Some f -> f conn_id {messageID=0;protocolOp=Unbind_request;controls=None}
		   | None -> ());
		(* remove the client from our table of clients, and
		   from the list of readable/writable fds, that way we
		   don't try to do a write to them, even though we may
		   have pending writes *)
		Hashtbl.remove si.si_client_sockets fd;
		reading := List.filter ((<>) fd) !reading;
		writing := List.filter ((<>) fd) !writing;
		excond := List.filter ((<>) fd) !excond
	  else (* a new connection has come in, accept it *)
	    let (newfd, sockaddr) = accept fd in
	    let rb = readbyte_of_fd newfd in
	      Hashtbl.add si.si_client_sockets newfd (allocate_connection_id si, [], rb)
	in
	  (* service connections which are ready to be read *)
	  List.iter process_read !reading;
	  List.iter (* service connections which are ready to be written to *)
	    (fun (fd: file_descr) ->
	       if Hashtbl.mem si.si_client_sockets fd then
		 let (conn_id, pending_ops, rb) = Hashtbl.find si.si_client_sockets fd in
		 let rec perform_writes si conn_id pending_ops rb =
		   match pending_ops with
		       [] -> 
			 print_endline "finished processing writes";
			 Hashtbl.replace si.si_client_sockets fd (conn_id, [], rb)
		     | hd :: tl -> 
			 try hd () with Finished ->
			   print_endline "processed write";
			   Hashtbl.replace si.si_client_sockets fd (conn_id, tl, rb);
			   perform_writes si conn_id tl rb
		 in
		   try perform_writes si conn_id pending_ops rb
		   with Server_error "data cannot be written" ->
		     (match si.si_backend.bi_op_unbind with
			  Some f -> f conn_id {messageID=0;protocolOp=Unbind_request;controls=None}
			| None -> ());
		     print_endline "closing client connection on bad write";
		     Hashtbl.remove si.si_client_sockets fd;
		     reading := List.filter ((<>) fd) !reading;
		     writing := List.filter ((<>) fd) !writing;
		     excond := List.filter ((<>) fd) !excond
	       else raise (Server_error "socket to write to not found"))
	    !writing;
	  List.iter process_read !excond (* Process out of band data*)
    done
