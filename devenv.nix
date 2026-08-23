{ pkgs, lib, config, inputs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.vscode-css-languageserver
    pkgs.zlib
    pkgs.ormolu
    pkgs.just
    pkgs.dart-sass
  ];

  # https://devenv.sh/languages/
  languages.haskell = {
    enable       = true;
    cabal.enable = true;
    lsp.enable   = true;
    stack.enable = false;
  };

  # https://devenv.sh/processes/
  # processes.dev.exec = "${lib.getExe pkgs.watchexec} -n -- ls -la";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  # scripts.hello.exec = ''
  #   echo hello from $GREET
  # '';

  # https://devenv.sh/basics/
  enterShell = ''
    export PATH=$PATH:~/.local/bin
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  # enterTest = ''
  #   echo "Running tests"
  #   git --version | grep --color=auto "${pkgs.git.version}"
  # '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
