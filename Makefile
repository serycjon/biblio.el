EMACS ?= emacs
CASK = env --unset INSIDE_EMACS cask

all: depends elc

# Ignores failures, since dependences of ‘biblio’ are locally satisfied
depends:
	$(CASK) install || true

elc:
	$(CASK) build

clean:
	$(CASK) clean-elc

test: clean # Must run clean to make tests work
	$(CASK) exec buttercup -L . -L tests

version:
	$(CASK) exec $(EMACS) --version
