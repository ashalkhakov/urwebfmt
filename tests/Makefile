%.ur.chk: %.ur
	./runtest.sh $< > $@

.PHONY: chks
chks: arith.ur.chk funs.ur.chk letinend.ur.chk conpat.ur.chk indents.ur.chk

cleanall:
	rm -f *.out *.chk *.exe
