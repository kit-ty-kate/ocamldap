-include Makefile.conf

SOURCES=lber.mli lber.ml ldap_types.mli ldap_types.ml ldap_error.mli	\
ldap_error.ml ldap_protocol.mli ldap_protocol.ml ulist.ml		\
ldap_urllexer.mll ldap_url.mli ldap_url.ml ldap_filterparser.mly	\
ldap_filterlexer.mll ldap_filter.mli ldap_filter.ml ldap_funclient.mli	\
ldap_funclient.ml ldap_schemalexer.mll ldap_schemaparser.mli		\
ldap_schemaparser.ml ldap_ooclient.mli ldap_ooclient.ml ldif_parser.ml	\
ldif_oo.ml ldif_oo.mli ldap_funserver.mli ldap_funserver.ml		\
ldap_dnlexer.mll
RESULT=ocamldap
PACKS=netstring str ssl

LIBINSTALL_FILES=$(wildcard *.mli *.cmi *.cma *.cmxa *.a *.so)
OCAMLDOCFLAGS=-colorize-code

all: debug-code-library
opt: native-code-library
reallyall: byte-code-library native-code-library
install: libinstall
uninstall: libuninstall

documentation:
	ocamlfind ocamldoc -html -d doc -package netstring,str,ssl lber.mli ldap_types.mli ldap_error.mli ldap_protocol.mli ldap_url.mli ldap_filter.mli ldap_funclient.mli ldap_ooclient.mli ldap_schemaparser.mli ldap_funserver.mli ldif_oo.mli


-include OCamlMakefile
