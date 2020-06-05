EXE := ray
GHC_OPTIONS := --ghc-options='-ferror-spans -fhide-source-paths -fprof-auto -rtsopts' # -fprint-unicode-syntax

dev: all
	ghcid --command="cabal new-repl $(GHC_OPTIONS)" | source-highlight -s haskell -f esc
repl:
	cabal new-repl $(GHC_OPTIONS)
all:
	cabal new-build $(GHC_OPTIONS) all
run:
	cabal new-run $(EXE)
runp:
	cabal new-run $(EXE) -- +RTS -p -s -l -hy -RTS
clean: cleanp
	cabal new-clean
cleanp:
	rm -f $(EXE).ps $(EXE).hp $(EXE).prof $(EXE).eventlog $(EXE).aux
check:
	cabal new-check
test:
	cabal new-test
docs:
	cabal new-haddock
ps:
	hp2ps -d -c $(EXE).hp
tags:
	rm -f tags codex.tags
	codex update --force
	haskdogs --hasktags-args "-b"
prof:
	cabal new-configure --enable-profiling
noprof:
	cabal new-configure --disable-profiling
hoogle:
	hoogle server --local

.PHONY: dev repl cleanp all run check test docs tags ps prof noprof hoogle
