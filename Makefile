elc:
	cask build

# Ignores failures, since dependences of ‘biblio’ are locally satisfied
depends:
	cask install || true

clean:
	cask clean-elc

test: clean elc
	cask exec buttercup -L . -L tests
