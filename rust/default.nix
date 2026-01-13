{
  perSystem = { config, pkgs, ... }:
    let
      cargoToml = builtins.fromTOML (builtins.readFile ./Cargo.toml);
      rust-toolchain = pkgs.symlinkJoin {
        name = "rust-toolchain";
        paths = [ pkgs.rustc pkgs.cargo pkgs.cargo-watch pkgs.rust-analyzer pkgs.rustPlatform.rustcSrc ];
      };
    in
    {
      # Rust package
      packages.hello_rust = pkgs.rustPlatform.buildRustPackage {
        inherit (cargoToml.package) name version;
        src = ./.;
        cargoLock.lockFile = ./Cargo.lock;
        nativeBuildInputs = with pkgs; [ pkg-config ];
        buildInputs = with pkgs; [
          nettle
          gmp
        ] ++ (if pkgs.stdenv.isDarwin then [ pkgs.fixDarwinDylibNames ] else [ ]);
        postInstall = ''
          ${if pkgs.stdenv.isDarwin then "fixDarwinDylibNames" else ""}
        '';
      };

      # Rust dev environment
      devShells.rust = pkgs.mkShell {
        shellHook = ''
          # For rust-analyzer 'hover' tooltips to work.
          export RUST_SRC_PATH=${pkgs.rustPlatform.rustLibSrc}
        '';
        nativeBuildInputs = with pkgs; [
          just
          rust-toolchain
          pkg-config
        ];
        buildInputs = with pkgs; [
          nettle
          gmp
        ];
        RUST_BACKTRACE = 1;
      };

      # Add your auto-formatters here.
      # cf. https://numtide.github.io/treefmt/
      treefmt.config = {
        programs.rustfmt.enable = true;
      };
    };
}
