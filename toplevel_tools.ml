#use "topfind";;
#require "ocamldap";;

open Ldap_ooclient;;
open Ldap_types;;
open Ldif_oo;;

let ldap_config_readhost = "ldap://odir.csun.edu";;
let ldap_config_writehost = "ldap://mallard.csun.edu";;
let ldap_config_base = "o=csun";;
let ldap_config_readdn = "cn=rmwd read,ou=proxies,ou=auth,o=csun";;
let ldap_config_writedn = "cn=directory manager,o=csun";;

let print_entries es = 
  let ldif = new ldif () in
    List.iter
      (fun e -> ldif#write_entry e)
      es
;;

let ldap_cmd_harness ~h ~d ~w f = 
  let ldap = new ldapcon [h] in
    try
      ldap#bind d ~cred:w;
      let res = f ldap in
	ldap#unbind;
	res
    with exn -> ldap#unbind;raise exn
;;

let ldapsearch ?(s=`SUBTREE) 
               ?(a=[]) ?(b=ldap_config_base) 
	       ?(d=ldap_config_searchdn) ?(w="") 
	       ?(h=ldap_config_readhost) filter =
  ldap_cmd_harness ~h ~d ~w
    (fun ldap -> 
       ldap#search 
	 ~base:b ~scope:s 
	 ~attrs:a filter)
;;

let ldapsearch_p ?(s=`SUBTREE) 
                 ?(a=[]) ?(b=ldap_config_base) 
		 ?(d=ldap_config_searchdn) ?(w="") 
		 ?(h=ldap_config_readhost) filter =
  print_entries (ldapsearch ~s ~a ~b ~h ~d ~w filter)
;;

let ldapmodify ?(h=ldap_config_writehost) ?(d=ldap_config_writedn) ~w dn mods = 
  ldap_cmd_harness ~h ~d ~w 
    (fun ldap -> ldap#modify dn mods)
;;
