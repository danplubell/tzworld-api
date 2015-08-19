.PHONY: all bench build clean configure install repl run 

all: install configure build 


build:
	cabal build --jobs

clean:
	cabal clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	cabal configure --enable-benchmarks --enable-tests


install:
	cabal sandbox init
	cabal install --enable-benchmarks --enable-tests --jobs --only-dependencies --reorder-goals

repl:
	cabal repl lib:tzworld-api

run:
	cabal run --jobs tzworld-api

