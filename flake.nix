{
  description = "Prototype of Pisa and related thesis working files";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/x86_64-linux";

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";

    infuse.url = "git+https://codeberg.org/Kirens/infuse.nix.git";
    infuse.inputs.nixpkgs-lib.follows = "nixpkgs";

    roughspec.url = "github:solrun/quickspec";
    roughspec.flake = false;

    devenv.url = "github:cachix/devenv";
    devenv.inputs = {
      flake-compat.follows = "";
      # All patches for devenv are upstreamed, prefer shared nixpkgs
      nixpkgs.follows = "nixpkgs";
      # We don't need the devenv binary
      nix.follows = "";
      cachix.follows = "";
    };
    devenv-root.url = "file+file:///dev/null";
    devenv-root.flake = false;
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = inputs:
    let mkFlake = inputs.flake-parts.lib.mkFlake { inherit inputs; };
    in mkFlake ({ inputs, ... }: {
      systems = import inputs.systems;
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.devenv.flakeModule
      ];
      perSystem = { self', lib, pkgs, ... }:
        let
          infuse = import inputs.infuse { inherit lib; };
          ghcVer = "ghc912";
          hPkgs = customHaskell.packages.${ghcVer};
          customHaskell = infuse pkgs.haskell {
            packages.${ghcVer}.__input.overrides.__overlay = (hfinal: hprev:
              let
                inherit (hfinal) callCabal2nix;
                hlib = pkgs.haskell.lib;
              in
              {
                pisa-base = callCabal2nix "pisa" ./project { };

                pisa-lib = hlib.setBuildTargets hfinal.pisa-base [
                  "lib:pisa"
                  "test-pisa"
                ];

                pisa = hlib.overrideCabal hfinal.pisa-base (old: {
                  buildTarget = "exe:pisa test-pisa";
                  isLibrary = false;
                  enableSharedExecutables = true;
                  enableSharedLibraries = true;
                  enableStaticLibraries = false;
                  enableLibraryProfiling = false;
                  buildDepends = [ pkgs.makeBinaryWrapper ];
                  postInstall =
                    let
                      ghcRuntime = hfinal.ghcWithPackages (_: lib.concatLists [
                        old.executableHaskellDepends
                        old.libraryHaskellDepends
                        [hfinal.pisa-lib]
                      ]);
                    in ''
                      wrapProgram $out/bin/pisa \
                        --set GHC_PACKAGE_PATH "${ghcRuntime}/lib/${ghcRuntime.meta.name}/lib/package.conf.d"
                    '';
                });

                quickspec = callCabal2nix "quickspec" inputs.roughspec { };

                twee-lib = pkgs.haskell.lib.overrideCabal hprev.twee-lib {
                  version = "2.4.2";
                  sha256 = "sha256-qXH8LIrBzJPvAZgulVi5/NjjHznhLGaLhzCtpQTDzLo=";
                  doCheck = false;
                };
              }

            );
          };

          addNeqPy = pkgs.writeText "extend.py" /*python*/ ''
            import fontforge
            import sys

            font = fontforge.open(sys.argv[1])

            eql = "equal.svg"
            font[0x3D].export(eql)
            slsh = "slash.svg"
            font[0x2F].export(slsh)

            neq = "neq.svg"
            nat = "nat.svg"
            with (open(eql, 'r') as eq,
                 open(slsh, 'r') as sl,
                 open(neq, 'w') as ne,
                 open(nat, 'w') as na):
                e = eq.readlines()
                ne.writelines(e[:-1] + sl.readlines()[3:])
                na.writelines(e[0:4] +
                  ['d="m 68,167 c -15,0 -40,0 -40,30 0,31 24,31 40,31 H 93 V 717 H 68 c -15,0 -40,0 -40,30 0,31 24,31 40,31 h 106 c 15,0 40,0 40,-30 0,-31 -24,-31 -40,-31 H 149 V 359 h 1 l 0,0 L 291,742 c 13,35 29,36 55,36 h 43 c 30,0 42,-3 42,-41 V 228 h 25 c 15,0 40,0 40,-30 0,-31 -24,-31 -40,-31 H 350 c -16,0 -40,0 -40,31 0,30 25,30 40,30 h 25 v 358 h -1 L 233,203 c -12,-33 -27,-36 -54,-36 H 136 Z M 188,228 368,717 H 336 L 156,228 Z" />\n</svg>'])

            with open(nat, 'r') as n:
                print(n.readlines()[:])

            neqG = font.createChar(0x2260, "Not Equal To")
            neqG.importOutlines(neq)
            natG = font.createChar(0x2115, "Double-Struck Capital N")
            natG.importOutlines(nat)
            if not (neqG.changed and natG.changed):
              exit(1)

            font.generate(sys.argv[2])
          '';

          extendNCM = base:
            let self = { tex = self; } // pkgs.runCommand base.name
              { inherit (base) pname; }
              ''
                fontdir="/fonts/opentype/public/newcomputermodern"

                ${pkgs.fontforge}/bin/fontforge -script "${addNeqPy}" \
                  "${base}$fontdir/NewCMMono10-Regular.otf" tmp.otf

                cp -r "${base}" "$out"
                fontdst="$out$fontdir"
                chmod +w "$fontdst"
                cp -f "tmp.otf" "$fontdst/NewCMMono10-Regular.otf"
              '';
            in self;

          texDist = pkgs.texlive.withPackages (ps: with ps; [
            xetex

            scheme-bookpub
            biblatex-apa

            collection-latexrecommended
            collection-mathscience

            (extendNCM newcomputermodern.tex)
            alegreya
            cleveref
            crossreftools
            csquotes
            datetime2
            datetime2-english
            moreverb
            newunicodechar
            relsize
            sansmathfonts
            seqsplit
            subfiles
            todonotes
            upquote
            wrapfig
            ulem
            capt-of
            esint
            dejavu
            sourcecodepro
            tikzmark
            tikz-decofonts
          ]);

          pdfBase = pkgs.callPackage ({ src, name, build ? "build/Main.pdf"
                                      , stdenv, inkscape, texliveDist }:
            stdenv.mkDerivation {
              inherit src;
              name = "${name}.pdf";
              nativeBuildInputs = [ texliveDist inkscape ];
              preBuildPhases = [ "preBuildPhase" ];
              preBuildPhase = ''export XDG_CACHE_HOME="$(mktemp -d)"'';
              installPhase = "cp ${build} $out";
            }
          ) { texliveDist = texDist; name = abort "name"; src = abort "src"; };

          devenvSetup = { config, ... }: {
            enterShell = "export name=${lib.escapeShellArg config.name}";
            devenv.root =
              let root = builtins.readFile inputs.devenv-root.outPath;
              in lib.mkIf (root != "") root;
          };
        in
        {

          overlayAttrs.haskell = customHaskell;

          devenv.modules = [ devenvSetup ];

          packages = {
            default = self'.packages.pisa;
            pisa-src = hPkgs.pisa-src;
            pisa = hPkgs.pisa;
            pisa-lib = hPkgs.pisa-lib;
            lean-toolchain = pkgs.callPackage
              ({ lean-toolchain-name, hash, lib, runCommand, fetchurl, zstd, clangStdenv, patchelf, runtimeShell }:
              let
                inherit (clangStdenv) cc;
                parsed = lib.splitString ":" lean-toolchain-name;
                repo = lib.elemAt parsed 0;
                tc = lib.elemAt parsed 1;
                tc' = lib.removePrefix "v" tc;
                final =
                  runCommand lean-toolchain-name
                  {
                    passthru = {
                      ELAN_TOOLCHAIN = lean-toolchain-name;
                      inherit root;
                    };
                  }
                  ''
                    TC=$out/toolchains/${lib.replaceStrings ["/"] ["--"] repo}---${tc}
                    mkdir -p $(dirname $TC)
                    ln -s ${root} $TC
                  '';
                root =
                  runCommand "${lean-toolchain-name}-root"
                  {
                    src = fetchurl {
                      url = "https://github.com/${repo}/releases/download/${tc}/lean-${tc'}-linux.tar.zst"; # XXX: only x86_64-linux
                      inherit hash;
                    };
                    nativeBuildInputs = [ patchelf zstd ];
                  }
                  ''
                    mkdir -p $out
                    tar --zstd -xf $src -C $out lean-${tc'}-linux --strip 1
                    cd $out/bin
                    rm llvm-ar
                    find . -type f -exec \
                      patchelf --set-interpreter $(cat ${cc}/nix-support/dynamic-linker) {} \;
                    ln -s ${cc}/bin/ar llvm-ar

                    mv leanc{,.orig}
                    cat > leanc << EOF
                    #! ${runtimeShell}
                    dir="\$(dirname "\''${BASH_SOURCE[0]}")"
                    # use bundled libraries, but not bundled compiler that doesn't know about NIX_LDFLAGS
                    LEAN_CC="\''${LEAN_CC:-${cc}/bin/cc}" exec -a "\$0" "\$dir/leanc.orig" "\$@" -L"\$dir/../lib"
                    EOF
                    chmod 755 leanc

                    mv lake{,.orig}
                    cat > cc << EOF
                    #! ${runtimeShell}
                    dir="\$(dirname "\''${BASH_SOURCE[0]}")"
                    # use bundled libraries, but not bundled compiler that doesn't know about NIX_LDFLAGS
                    exec "${cc}/bin/cc" "\$@" -L"\$dir/../lib"
                    EOF
                    cat > lake << EOF
                    #! ${runtimeShell}
                    dir="\$(dirname "\''${BASH_SOURCE[0]}")"
                    # use custom `cc`
                    LEAN_CC="\''${LEAN_CC:-\$dir/cc}" exec -a "\$0" "\$dir/lake.orig" "\$@"
                    EOF
                    chmod 755 lake cc
                  '';
              in final) {
                lean-toolchain-name = lib.trim (lib.readFile ./lean/lean-toolchain);
                hash = "sha256-5FRMRBcDd+8z1q4evn7qEDg/f+IKhNVsH9wY0W/MtXU=";
              };
            pisa-lean = pkgs.callPackage
              ({ lib, clangStdenv, lean-toolchain, makeWrapper, elan }:
                clangStdenv.mkDerivation (drv: {
                  name = "pisa-lean";
                  src = ./lean;
                  LAKE_HOME = "${lean-toolchain.passthru.root}";
                  buildPhase = "$LAKE_HOME/bin/lake build";
                  nativeBuildInputs = [ makeWrapper ];
                  installPhase = ''
                    mkdir -p $out/bin
                    install -t $out/bin .lake/build/bin/${drv.name}
                    wrapProgram $out/bin/${drv.name} \
                        --suffix PATH : ${lib.makeBinPath [ elan ]}
                  '';
                })
              ) { inherit (self'.packages) lean-toolchain; };

            report = pdfBase.override {
              name = "Pisa";
              src = ./writing/report;
            };
            presentation = pdfBase.override {
              name = "Presentation";
              src = ./writing/presentation;
            };
          };

          checks = {
            inherit (self'.packages) pisa;
          };

          devenv.shells.lean = {
            stdenv = pkgs.clangStdenv;
            packages = with self'.packages; [ lean-toolchain.passthru.root pisa ];
            enterShell = ''
              LAKE_HOME="${self'.packages.lean-toolchain.passthru.root}";
            '';
          };
          devenv.shells.writing = {
            enterShell = ''
              unset SOURCE_DATE_EPOCH # Use current time for in imperative shell
            '';
            packages = with pkgs; [
              texDist
              inotify-tools
              inkscape
            ];
          };
          devShells.pisa = hPkgs.shellFor {
            packages = p: with p; [ pisa ];
            withHoogle = true;
            buildInputs =
              (with pkgs; [
                self'.packages.pisa-lean
                #zlib # May be required for building some dependencies
                entr # Automatic reloading
                jq # JSON Query and reformater
                fx # JSON viewer
              ]) ++
              (with hPkgs; [
                haskell-language-server
                cabal-install
              ]);
          };
        };
    });
}
