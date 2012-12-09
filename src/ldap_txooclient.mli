module Make : functor (M : Ldap_types.Monad) -> sig

open Ldap_ooclient

(** the abstract type of a transaction *)
type txn

(** raised when a commit fails, contains a list of entries which were
    not rolled back successfully only if rollback failed as well,
    otherwise None *)
exception Txn_commit_failure of string * exn * Make(M).ldapentry_t list option

(** raised when an explicit rollback fails *)
exception Txn_rollback_failure of string * exn

(** A subclass of ldapcon which implements an experimental interface
    to draft_zeilenga_ldap_txn. A draft standard for multi object
    transactions over the ldap protocol. This class can only implement
    advisory transactions because it must depend on the advisory
    locking mechanisms for the transactions to be consistant. You use
    this class by calling begin_txn to get a transaction id, and then
    associating a set of ldapentry objects with the transaction by
    calling associate_entry_with_txn. You are then free to modify
    those entries in any way you like, and when you are done, you can
    either call commit_txn, or rollback_txn. Commit will commit the
    changes of all the entries associated with the transaction to the
    database. For other writers which obey advisory locking the commit
    operation is atomic. For readers which are willing to obey
    advisory locking is atomic. If the commit fails, a full rollback
    occurrs, including all changes made to the directory. For example
    in a set of N entries in a transaction, if the modificiation of
    the nth entry fails to commit, then the modifications to all the
    previous entries, which have already been made in the directory,
    are undone. It is important to note that if advisory locking is
    not obeyed, rollback may not be successful. Rollback undoes all
    the changes you've made in memory, and unlocks all the objects in
    the transaction. After a transaction object has been commited or
    rolled back it is considered "dead", and cannot be used again. *)
class ldapadvisorytxcon :
  ?connect_timeout:int ->
  ?referral_policy:[> `RETURN ] ->
  ?version:int ->
  string list -> string -> string -> string -> (* hosts binddn bindpw mutextbldn *)
  object
    method add : Make(M).ldapentry -> unit M.t
    method bind :
      ?cred:string -> ?meth:Ldap_funclient.Make(M).authmethod -> string -> unit M.t
    method delete : string -> unit M.t
    method modify :
      string ->
      (Ldap_types.modify_optype * string * string list) list -> unit M.t
    method modrdn : string -> ?deleteoldrdn:bool -> ?newsup:string option -> string -> unit M.t
    method rawschema : Make(M).ldapentry M.t
    method schema : Ldap_schema.schema M.t
    method search :
      ?scope:Ldap_types.search_scope ->
      ?attrs:string list ->
      ?attrsonly:bool -> ?base:string ->
      ?sizelimit:Int32.t -> ?timelimit:Int32.t ->
      string -> Make(M).ldapentry list M.t
    method search_a :
      ?scope:Ldap_types.search_scope ->
      ?attrs:string list ->
      ?attrsonly:bool -> ?base:string ->
      ?sizelimit:Int32.t ->  ?timelimit:Int32.t ->
      string -> (?abandon:bool -> unit -> Make(M).ldapentry M.t) M.t
    method unbind : unit M.t
    method update_entry : Make(M).ldapentry -> unit M.t
    method begin_txn : txn
    method associate_entry : txn -> Make(M).ldapentry_t -> unit
    method associate_entries : txn -> Make(M).ldapentry_t list -> unit
    method disassociate_entry : txn -> Make(M).ldapentry_t -> unit M.t
    method disassociate_entries : txn -> Make(M).ldapentry_t list -> unit M.t
    method commit_txn : txn -> unit M.t
    method rollback_txn : txn -> unit
  end

end
