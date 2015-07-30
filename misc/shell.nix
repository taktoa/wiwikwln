with import <nixpkgs> {};

let latexEnabled = true;
    latexPackages = texLiveAggregationFun {
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
  name = "wiwinwln-latest";
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
