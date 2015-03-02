all:
	bnfc -d Grammar.cf
	happy -gca Grammar/Par.y
	alex -g Grammar/Lex.x

clean:
	-rm -f Grammar/*.log Grammar/*.aux Grammar/*.hi Grammar/*.o Grammar/*.dvi
	-rm -f Grammar/Doc.ps

distclean: clean
	-rm -f Grammar/Doc.* Grammar/Lex.* Grammar/Par.* Grammar/Layout.* Grammar/Skel.* Grammar/Print.* Grammar/Test.* Grammar/Abs.* Grammar/Test Grammar/ErrM.* Grammar/SharedString.* Grammar/ComposOp.* Grammar/Grammar.dtd Grammar/XML.* Makefile*
		-rmdir -p Grammar/

