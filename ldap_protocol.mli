(* an implementation of the ldap wire protocol

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

(** an implementation of the ldap wire protocol *)

open Ldap_types
open Lber

(** given an ldap error code return a string describing it *)
val err2string :
  [> `ADMINLIMIT_EXCEEDED
   | `ALIAS_DEREF_PROBLEM
   | `ALIAS_PROBLEM
   | `ALREADY_EXISTS
   | `AUTH_METHOD_NOT_SUPPORTED
   | `BUSY
   | `COMPARE_FALSE
   | `COMPARE_TRUE
   | `CONFIDENTIALITY_REQUIRED
   | `CONSTRAINT_VIOLATION
   | `INAPPROPRIATE_AUTH
   | `INAPPROPRIATE_MATCHING
   | `INSUFFICIENT_ACCESS
   | `INVALID_CREDENTIALS
   | `INVALID_DN_SYNTAX
   | `INVALID_SYNTAX
   | `LOCAL_ERROR
   | `LOOP_DETECT
   | `NAMING_VIOLATION
   | `NOT_ALLOWED_ON_NONLEAF
   | `NOT_ALLOWED_ON_RDN
   | `NO_OBJECT_CLASS_MODS
   | `NO_SUCH_ATTRIBUTE
   | `NO_SUCH_OBJECT
   | `OBJECT_CLASS_VIOLATION
   | `OPERATIONS_ERROR
   | `OTHER
   | `PROTOCOL_ERROR
   | `REFERRAL
   | `SASL_BIND_IN_PROGRESS
   | `SERVER_DOWN
   | `SIZELIMIT_EXCEEDED
   | `STRONG_AUTH_REQUIRED
   | `SUCCESS
   | `TIMELIMIT_EXCEEDED
   | `TYPE_OR_VALUE_EXISTS
   | `UNAVAILABLE
   | `UNAVAILABLE_CRITICAL_EXTENSION
   | `UNDEFINED_TYPE
   | `UNWILLING_TO_PERFORM ] ->
  string

(** encode a value of type ldap_message using lber and return
  a string which is ready to be put on the wire *)
val encode_ldapmessage : ldap_message -> string

(** decode an ldap_message from the wire, and build/return a
  structure of type ldap_message *)
val decode_ldapmessage : readbyte -> ldap_message
