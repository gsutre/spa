# $Id: Makefile 3378 2015-11-15 15:29:52Z sutre $

# Configuration
BUILD_FLAGS = -use-menhir
DOC_FLAGS   = -charset UTF-8, -stars, -colorize-code, -sort,
DOC_FLAGS  += -hide Pervasives, -hide-warnings

# Default target
all: spa.d.byte

%.native %.byte %.d.byte %.cmo %.d.cmo: FORCE
	ocamlbuild $(BUILD_FLAGS) $@

doc: FORCE
	ocamlbuild $(BUILD_FLAGS) -docflags '$(DOC_FLAGS)' doc.docdir/index.html
	mv doc.docdir doc

test: FORCE runtests.d.byte
	CAMLRUNPARAM="b" ./runtests.d.byte

clean: FORCE
	ocamlbuild -quiet -clean

FORCE:

# Check for ocamlbuild
ifneq ($(shell ocamlbuild -version 2>/dev/null | sed -e 's/ .*//'), ocamlbuild)
  $(error Check for ocamlbuild failed.  Is OCaml installed?)
endif

# Check for menhir
ifneq ($(shell menhir --version 2>/dev/null | sed -e 's/,.*//'), menhir)
  $(error Check for menhir failed.  Is Menhir installed?)
endif

# Check for OCaml compiler >= 4.01.0 (required for Format.asprintf)
ifneq ($(shell expr $$(ocamlc -version) \>= 4.01.0), 1)
  $(error Check for ocamlc version >= 4.01.0 failed.)
endif
