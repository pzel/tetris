.PHONY: all ghci test

ghci:
	stack build
	stack ghci

all:
	stack build
	stack exec tetris-exe

test:
	stack test
