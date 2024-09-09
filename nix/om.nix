# Configuration for https://github.com/juspay/omnix
{
  flake.om.ci.default = {
    root = {
      dir = ".";
      steps = {
        # Users can define custom steps to run any arbitrary flake app or devShell command.
        custom = {
          # This is equivalent to `nix run .#hs-cac-client`
          hello-haskell = {
            type = "app";
            name = "hello-haskell";
          };
          # This is equivalent to `nix develop .#default -c cd haskell && cabal run`
          cabal-run = {
            type = "devshell";
            # Name of the devshell profile to use
            # name = "default";
            command = [ "just" "_update-and-run" ];
          };
        };
      };
    };
  };
}
