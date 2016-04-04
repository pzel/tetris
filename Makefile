.PHONY: all ghci play test
STACK := stack --verbosity warn

play:
	@$(STACK) build
	stack exec tetris-exe

ghci:
	@$(STACK) build
	@$(STACK) ghci

test:
	@$(STACK) test
