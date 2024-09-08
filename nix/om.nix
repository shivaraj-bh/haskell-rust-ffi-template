# Configuration for https://github.com/juspay/omnix
{
  flake.om.ci.default = {
    root = {
      dir = ".";
      steps = {
        # Users can define custom steps to run any arbitrary flake app or devShell command.
        custom = {
          # This equivalent to `nix run .#hs-cac-client`
          hello-haskell = {
            type = "app";
            name = "hello-haskell";
          };
        };
      };
    };
  };
}
