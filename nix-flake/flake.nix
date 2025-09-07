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
              extensions = [
                "rust-src"
                "rust-analyzer"
              ];
            })
            libiconvReal
            cmake
            ninja
            nodejs
            mandoc
            openssl
            tree
          ];

          env = {
            PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig";
          };

          shellHook = ''
            ln -sf $(rustc --print=sysroot) ./.direnv/rust
            DIR="$\{0:a:h}"

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

            export_alias_exec() {
              local name=$1
              local alias_dir=$PWD/.direnv/aliases
              mkdir -p "$alias_dir"
              PATH_add "$alias_dir"
              local target="$alias_dir/$name"
              echo "#!/usr/bin/env bash" > "$target"
              echo "$2 \$@" >> "$target"
              chmod +x "$target"
            }

            nrxc() {
                cargo nexus build-sys build --targets compiler $@
            }

            export_alias_exec nrx "$(realpath target/debug/nrx)"
            export_function nrxc

            nexus() {
                cargo nexus $@
            }

            export_function nexus
          '';
        };
      }
    );
}
