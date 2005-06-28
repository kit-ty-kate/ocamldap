open Ldap_types
open Ldap_ooclient

exception Ldap_mutex of string * exn

class type mutex_t =
object
  method lock: unit
  method unlock: unit
end

class mutex ldapurls binddn bindpw mutexdn = 
object (self)
  val ldap = 
    let ldap = new ldapcon ldapurls in
      ldap#bind binddn ~cred:bindpw;
      ldap

  method private addmutex = 
    let mt = new ldapentry in
    let mtrdn = List.hd (Ldap_dn.of_string mutexdn) in
      mt#set_dn mutexdn;
      mt#add [("objectclass", ["top";"mutex"]);
	      (mtrdn.attr_type, mtrdn.attr_vals)];
      try ldap#add mt
      with exn -> raise (Ldap_mutex ("addmutex", exn))

  method lock = 
    try
      let obj = 
	try
	  ldap#search 
	    ~base:mutexdn
	    ~scope:`BASE
	    "objectclass=*"
	with LDAP_Failure (`NO_SUCH_OBJECT, _, _) -> []
      in
	if List.length obj = 0 then begin
	  self#addmutex;
	  self#lock
	end
	else if List.length obj = 1 then
	  while true
	  do
	    try 
	      ldap#modify 
		(List.hd obj)#dn [(`ADD, "mutexlocked", ["locked"])];
	      failwith "locked"
	    with (* the mutex is locked already *)
		LDAP_Failure (`TYPE_OR_VALUE_EXISTS, _, _)
	      | LDAP_Failure (`OBJECT_CLASS_VIOLATION, _, _) -> ()
	  done
	else failwith ("huge error, multiple objects with the same dn")
    with
	Failure "locked" -> ()
      | (Ldap_mutex _) as exn -> raise exn
      | exn -> raise (Ldap_mutex ("lock", exn))

  method unlock =     
    try
      let obj = 
	try
	  ldap#search 
	    ~base:mutexdn
	    ~scope:`BASE
	    "objectclass=*"
	with LDAP_Failure (`NO_SUCH_OBJECT, _, _) -> []
      in
	if List.length obj = 0 then begin
	  self#addmutex;
	  self#unlock
	end
	else if List.length obj = 1 then
	  try 
	    ldap#modify 
	      (List.hd obj)#dn [(`DELETE, "mutexlocked", [])]
	  with LDAP_Failure (`NO_SUCH_ATTRIBUTE, _, _) -> ()
    with
	(Ldap_mutex _) as exn -> raise exn
      | exn -> raise (Ldap_mutex ("unlock", exn))
end
