{
  perSystem = { config, pkgs, self', ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      autoWire = [ "packages" "checks" "apps" ];
      settings = {
        hello_rust.custom = _: self'.packages.hello_rust;
      };
    };

    packages.default = self'.packages.hello-haskell;

    # Add your auto-formatters here.
    # cf. https://numtide.github.io/treefmt/
    treefmt.config = {
      programs = {
        cabal-fmt.enable = true;
        hlint.enable = true;
      };
    };

    devShells.haskell = pkgs.mkShell {
      name = "hello-haskell";
      inputsFrom = [
        config.haskellProjects.default.outputs.devShell
      ];
      # `*LD_LIBRARY_PATH` is needed for `cabal repl` to work, see: https://discourse.nixos.org/t/shared-libraries-error-with-cabal-repl-in-nix-shell/8921/10
      # Turns out, only `*LD_LIBRARY_PATH` is not enough for `ghcid` to work but `LIBRARY_PATH` seems
      # to work for both.
      shellHook = ''
        export LIBRARY_PATH=${config.haskellProjects.default.outputs.finalPackages.hello_rust}/lib
      '';
    };
  };
}
