all: Data.hs Main.hs Splendor.hs Search.hs
	ghc -O3 Main -threaded

duel: Data.hs Splendor.hs Search.hs Data.hs
	ghc -O3 Duel.hs -threaded
	./Duel

time: all
	echo Quit | time ./Main +RTS -N8

clean:
	rm *.o
	rm *.hi

prof: Data.hs Main.hs Splendor.hs Search.hs Data.hs
	nix-shell -p "(haskell.packages.ghc822.extend (self: super: {mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });})).ghcWithPackages (p: with p; [parallel random])" -j4 --run 'ghc -O3 Main.hs -threaded -prof -fprof-auto -fprof-cafs'
	echo Quit | ./Main +RTS -N8 -p

prof-duel: Duel.hs Splendor.hs Search.hs Data.hs
	nix-shell -p "(haskell.packages.ghc822.extend (self: super: {mkDerivation = expr: super.mkDerivation (expr // { enableLibraryProfiling = true; });})).ghcWithPackages (p: with p; [parallel random])" -j4 --run 'ghc -O3 Duel.hs -threaded -prof -fprof-auto -fprof-cafs'
	./Duel +RTS -N8 -p -xc
