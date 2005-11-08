-include Makefile.conf

SOURCES=lber.mli lber.ml ldap_types.mli ldap_types.ml ldap_error.mli	\
ldap_error.ml ldap_protocol.mli ldap_protocol.ml ulist.ml		\
ldap_urllexer.mll ldap_url.mli ldap_url.ml ldap_filterparser.mly	\
ldap_filterlexer.mll ldap_filter.mli ldap_filter.ml ldap_funclient.mli	\
ldap_funclient.ml ldap_schemalexer.mll ldap_schema.mli ldap_schema.ml	\
ldap_dnparser.mly ldap_dnlexer.mll ldap_dn.mli ldap_dn.ml		\
ldap_ooclient.mli ldap_ooclient.ml ldap_schemacheck.ml ldap_mutex.mli	\
ldap_mutex.ml ldap_txooclient.mli ldap_txooclient.ml ldif_parser.ml	\
ldif_oo.ml ldif_oo.mli ldap_funserver.mli ldap_funserver.ml		\
ldif_changerec_parser.mly ldif_changerec_lexer.mll			\
ldif_changerec_oo.mli ldif_changerec_oo.ml ldap_toplevel.ml
RESULT=ocamldap
PACKS=netstring str ssl
#OCAMLFLAGS=-rectypes

LIBINSTALL_FILES=$(wildcard *.mli *.cmi *.cma *.cmxa *.a *.so *.o *.cmx ldap_toplevel.cmo)
OCAMLDOCFLAGS=-colorize-code

all: debug-code-library
opt: native-code-library
reallyall: byte-code-library native-code-library
install: libinstall
uninstall: libuninstall

documentation:
	ocamlfind ocamldoc -d doc/ocamldap/html -colorize-code -html -package netstring,str,ssl lber.mli ldap_types.mli ldap_error.mli ldap_protocol.mli ldap_url.mli ldap_filter.mli ldap_dn.mli ldap_funclient.mli ldap_ooclient.mli ldap_schemaparser.mli ldap_funserver.mli ldif_oo.mli ldap_toplevel.mli ldap_mutex.mli ldif_changerec_oo.mli ldap_txooclient.mli

-include OCamlMakefile
