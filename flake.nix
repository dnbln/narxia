{
  description = "Narxia devshell";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      {
        devShells.default = with pkgs; mkShell {
          buildInputs = [
            pkg-config
            zstd
            (rust-bin.nightly.latest.default.override {
              extensions = [ "rust-src" ];
            })
            libiconvReal
            cmake
            ninja
            nodejs
            mandoc
          ];

          shellHook = ''
            ln -sf $(rustc --print=sysroot) ./.direnv/rust

            export_function() {
              local name=$1
              local alias_dir=$PWD/.direnv/aliases
              mkdir -p "$alias_dir"
              PATH_add "$alias_dir"
              local target="$alias_dir/$name"
              if declare -f "$name" >/dev/null; then
                echo "#!/usr/bin/env bash" > "$target"
                declare -f "$name" >> "$target" 2>/dev/null
                echo "$name \$@" >> "$target"
                chmod +x "$target"
              fi
            }

            nrx() {
                $PWD/target/debug/nrx $@
            }

            nrxc() {
                cargo nexus build-sys build --targets compiler $@
            }

            export_function nrx
            export_function nrxc
          '';
        };
      }
    );
}
