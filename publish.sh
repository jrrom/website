#!/usr/bin/env bash

# Note: The user must be in the Nix development environment.

cabal run site clean
cabal run site build

git add -A
git commit
