urweb.grm.sig urweb.grm.sml: urweb.grm
	mlyacc $<

urweb.lex.sml: urweb.lex
	mllex $<

SOURCES=$(wildcard *.sml) $(wildcard *.sig)

urwebfmt: urweb.grm.sig urweb.grm.sml urweb.lex.sml $(SOURCES)
	mlton urwebfmt.mlb

tests: urwebfmt
	make -C tests cleanall
	make -C tests
