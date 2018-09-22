urweb.grm.sig urweb.grm.sml: urweb.grm
	mlyacc $<

urweb.lex.sml: urweb.lex
	mllex $<

urwebfmt: urweb.grm.sig urweb.grm.sml urweb.lex.sml parse_print.sml main.sml
	mlton sources.mlb
