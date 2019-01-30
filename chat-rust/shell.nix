let
  moz_overlay = import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz);
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  rustsrc = nixpkgs.latest.rustChannels.stable.rust-src;
in
  with nixpkgs;
  stdenv.mkDerivation {
    name = "moz_overlay_shell";
    buildInputs = [
      nixpkgs.latest.rustChannels.stable.rust
      rustsrc
      ];
    RUST_SRC_PATH="${rustsrc}/lib/rustlib/src/rust/src";
  }
