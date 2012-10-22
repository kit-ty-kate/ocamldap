(* OASIS_START *)
(* DO NOT EDIT (digest: 6d0cf80cc420b709157070a37d12c56b) *)

ocamldap - Ocamldap is an implementation of the Light Weight Directory Access Protocol
======================================================================================

See the file [INSTALL.txt](INSTALL.txt) for building and installation
instructions.

Copyright and license
---------------------

ocamldap is distributed under the terms of the GNU Lesser General Public
License version 2.1.

(* OASIS_STOP *)

---------------------------------------------------------------------------
Synopsis
---------------------------------------------------------------------------

Ocamldap is an ldap toolkit.  It can be used by ocaml programs to
communicate with ldap servers, and to build your own ldap servers.

---------------------------------------------------------------------------
Contents
---------------------------------------------------------------------------
Changes              - History of code changes.
INSTALL              - Short notes on compiling and installing the library
Makefile             - Top Makefile
README               - this file

---------------------------------------------------------------------------
Prerequisites
---------------------------------------------------------------------------

1) findlib
2) ocamlnet
3) ocaml-ssl

---------------------------------------------------------------------------
Features
---------------------------------------------------------------------------
* Ocamldap supports the core ldap-client functions, including search, add,
  modify, and delete.  
* object oriented interface with additional features.
  Such as, nice data structures for local ldap entries which
  record local modifications and can sync them with the server, fewer
  arguments needed to perform simple tasks, and transparent reconnection
  of dropped connections.
* Ocamldap includes an ldif parser, which allows you to read ldif files into
  entry objects. It also supports ldif change records.
* Ocamldap has a method call to grab the schema of an ldapv3 server
* Basic ldap server functionality (ldap_funserver) allows you to easily
  construct your own ldap servers. Perfect for meta directories, 
  and other cool projects. Someday maybe your main database :-)

---------------------------------------------------------------------------
Known Bugs/Missing Features
---------------------------------------------------------------------------

---------------------------------------------------------------------------
Obtaining ocamldap
---------------------------------------------------------------------------
Up to date info, can be found at

http://ocamldap.sourceforge.net

---------------------------------------------------------------------------
Contact
---------------------------------------------------------------------------
Please send questions or comments to eric.stokes@csun.edu.
I hope you find ocamldap useful!
