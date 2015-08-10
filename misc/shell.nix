# -*- mode: nix -*-
# What I Wish I Knew When Learning Nix - misc/shell.nix
#
# A Nix expression that encodes the compile-time dependencies of
# building the website.
#
# Written in 2015 by Charles Strahan <charles.c.strahan@gmail.com>
#
# To the extent possible under law, the author(s) have dedicated all
# copyright and related and neighboring rights to this software to the
# public domain worldwide. This software is distributed without any
# warranty.  You should have received a copy of the CC0 Public Domain
# Dedication along with this software. If not, see
# <http://creativecommons.org/publicdomain/zero/1.0/>.


with import <nixpkgs> {};

{ latexEnabled ? true }:

let latexPackages = texLiveAggregationFun {
      paths = [
        texLive
        texLiveExtra
        texLiveCMSuper
        lmodern
        tipa
      ];
    };
    ghcPackages = haskellngPackages.ghcWithPackages (p: with p; [
      pandoc
      pandoc-types
    ]);
in
stdenv.mkDerivation rec {
  name = "wiwikwln-latest";
  buildInputs = with pkgs; ([
    ghcPackages
  ] ++ (if latexEnabled then [ latexPackages ] else []));
  
  shellHook = ''
      export NIX_GHC="${ghcPackages}/bin/ghc"
      export NIX_GHCPKG="${ghcPackages}/bin/ghc-pkg"
      export NIX_GHC_DOCDIR="${ghcPackages}/share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$($NIX_GHC --print-libdir)
  '';
}
