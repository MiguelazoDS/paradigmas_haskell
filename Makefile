make:
		ghc script.hs
		ghc call\ tree.hs
clean:
		rm -f *.o
		rm -f *.hi
		rm -f script
		rm -f call\ tree
		rm -f Tree
