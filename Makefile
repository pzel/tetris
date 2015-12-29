.PHONY: all ghci play test
STACK := stack --verbosity warn

play:
	@$(STACK) build
	-@ # echo in case we're running inside a dumb terminal:
	@$$COLORTERM -e 'stack exec tetris-exe'

ghci:
	@$(STACK) build
	@$(STACK) ghci

test:
	@$(STACK) test
