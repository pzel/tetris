.PHONY: all ghci play test

play:
	@stack build
	-@ # echo in case we're running inside a dumb terminal:
	@$$COLORTERM -e 'stack exec tetris-exe'

ghci:
	stack build
	stack ghci

test:
	stack test
