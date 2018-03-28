all: Data.hs Main.hs Splendor.hs Search.hs Eval.hs
	ghc -O3 Main -threaded

clean:
	rm *.o
	rm *.hi

prof: Data.hs Main.hs Splendor.hs Search.hs Eval.hs
	ghc -O3 Main -threaded -prof -fprof-auto -fprof-cafs
	echo Quit | ./Main +RTS -N8 -p
