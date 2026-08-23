# management
clean:
    cabal clean
    cabal run website clean
    rm -rf {{justfile_directory()}}/dist-newstyle

deep-clean: clean
    rm -rf ~/.cache/hie-bios \
           ~/.cache/ghcide \
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
