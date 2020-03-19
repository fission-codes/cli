package = fission-cli

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build --fast $(package):lib

release:
	$(stack) build

dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

profile:
	$(stack) --work-dir .stack-work-profiling --profile build --fast

install:
	$(stack) install --fast

ghci:
	$(stack) repl $(package):lib --no-build --no-load --ghci-options='-j6 +RTS -A128m'

linter:
	$(stack) test :fission-lint --fast

docs:
	$(stack) haddock $(package) --open

docserver:
	http-server ./.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/doc/html/$(package) -p 1313 & \
    open http://localhost:1313

quality:
	$(stack) build --test --fast $(package)

doctest:
	$(stack) test :fission-doctest --fast

testsuite:
	$(stack) test :fission-test --fast

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

bench:
	$(stack) build --fast --bench $(package)

dev:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --main-is $(package):fission-cli-exe"

setup:
	stack install ghcid && stack install yesod-bin

.PHONY : build dirty run install ghci test test-ghci watch
