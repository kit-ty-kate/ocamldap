
(** functions for using ocamldap on the toplevel. Designed to be as
close to the OpenLDAP command line tools as possible. These are a work
in progress *)

(** print out a list of ldapentries *)
val print_entries : Ldap_ooclient.ldapentry list -> unit

(** connect to an ldap server, perform a search, disconnect, and
    return a list of search results.

    @param s define the search scope, the default is `SUBTREE @param a
    define a list of attributes to return, the default is to return all
    attributes, which is encoded as [] @param b define the search base,
    the default is "" @param d define the dn to bind with, the default is
    "", which means to bind anonymously @param w define the credentials to
    bind with, the default is "", which will work for an anonymous bind
    @param h the ldapurl of the host/port to connect to

    The last argument is the search filter.

    @raise LDAP_Failure
*)
val ldapsearch :
  ?s:Ldap_types.search_scope ->
  ?a:string list ->
  ?b:string ->
  ?d:string ->
  ?w:string -> h:string -> string -> Ldap_ooclient.ldapentry list

(** connect to an ldap server, perform a search, disconnect, and
    print out the list of search results, returning unit.

    @param s define the search scope, the default is `SUBTREE @param a
    define a list of attributes to return, the default is to return all
    attributes, which is encoded as [] @param b define the search base,
    the default is "" @param d define the dn to bind with, the default is
    "", which means to bind anonymously @param w define the credentials to
    bind with, the default is "", which will work for an anonymous bind
    @param h the ldapurl of the host/port to connect to

    The last argument is the search filter.

    @raise LDAP_Failure
*)
val ldapsearch_p :
  ?s:Ldap_types.search_scope ->
  ?a:string list ->
  ?b:string -> ?d:string -> ?w:string -> h:string -> string -> unit

(** connect to an ldap server, and perform a modification. This
    function will be changed in the future to take a list of
    modifications, they type will probably be (string,
    (Ldap_types.modify_optype * string * string list)) list, that way you
    can modify multiple dns in one shot, just like on the command line.

    @param d define the dn to bind with, the default is "", which means to bind
    anonymously
    @param w define the credentials to bind with, the default is "", which will
    work for an anonymous bind
    @param h the ldapurl of the host/port to connect to

    The string argument is the dn you want to modify

    @raise LDAP_Failure
*)
val ldapmodify :
  h:string ->
  d:string ->
  w:string ->
  string -> (Ldap_types.modify_optype * string * string list) list -> unit

(** connect to an ldap server, and add the specified ldapentries.

    @param d define the dn to bind with, the default is "", which means to bind
    anonymously
    @param w define the credentials to bind with, the default is "", which will
    work for an anonymous bind
    @param h the ldapurl of the host/port to connect to

    @raise LDAP_Failure
*)
val ldapadd :
  h:string -> d:string -> w:string -> Ldap_ooclient.ldapentry list -> unit
