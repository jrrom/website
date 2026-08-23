# management
clean:
    cabal clean
    cabal run website clean

deep-clean: clean
    rm -rf ~/.cache \
           ~/.hie
    @echo "Clear and ready to go"

# development
build:
    cabal run website build

watch:
    cabal run website watch

# deployment
publish message: clean build
    git add -A
    git commit -m {{message}}
