all: Data.hs Main.hs Splendor.hs Search.hs
	ghc -O3 Main -threaded

clean:
	rm *.o
	rm *.hi


