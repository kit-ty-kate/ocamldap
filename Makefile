-include Makefile.conf

SOURCES=lber.mli lber.ml ldap_types.ml ldap_protocol.mli		\
ldap_protocol.ml ulist.ml ldap_urllexer.mll ldap_url.mli ldap_url.ml	\
ldap_filterparser.mly ldap_filterlexer.mll ldap_filter.mli		\
ldap_filter.ml ldap_funclient.mli ldap_funclient.ml			\
ldap_schemalexer.mll ldap_schemaparser.ml ldap_ooclient.mli		\
ldap_ooclient.ml ldif_parser.ml ldif_oo.ml ldif_oo.mli			\
ldap_funserver.mli ldap_funserver.ml
RESULT=ocamldap
PACKS=netstring
OCAMLNCFLAGS=-inline 1000

LIBINSTALL_FILES=$(wildcard *.mli *.cmi *.cma *.cmxa *.a *.so)
OCAMLDOCFLAGS=-colorize-code $(wildcard *.mli)

# link to ssl if it is present, otherwise disable it
ifneq ($(strip $(shell ocamlfind query ssl)),)
	PPFLAGS+=-DSSL
	PACKS+=ssl
endif

all: byte-code-library clean-doc htdoc
opt: native-code-library
reallyall: byte-code-library native-code-library
install: libinstall
uninstall: libuninstall

-include OCamlMakefile
