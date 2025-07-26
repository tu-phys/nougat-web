{
  stdenv,
  lib,
  roswell,
  sbcl,
  which,
  libyaml,
  sbclPackages,
  fetchFromGitHub,
  bash,
  pkgs,
}:

let
  str = sbcl.buildASDFSystem rec {
    pname = "str";
    version = "0.21";
    src = fetchFromGitHub {
      owner = "vindarel";
      repo = "cl-str";
      tag = "${version}";
      hash = "sha256-lS6du4Q9iPOiRFb67JYP9P6v8LtG+55HYl4eObePNmU=";
    };
    lispLibs = [
      sbclPackages.cl-ppcre
      sbclPackages.cl-ppcre-unicode
      sbclPackages.cl-change-case
    ];
  };
  pkg = (
    sbcl.buildASDFSystem {
      pname = "nougat-web";
      version = "1.0.0";
      src = ./.;

      meta.mainProgram = "nougat-web";
      lispLibs = with sbclPackages; [
        str
        ningle
        clack
        cl-who
        log4cl
        envy
        cl-fad
        sbclPackages."_3bmd"
        dexador
        cl-interpol
        cl-json
        arrow-macros
        serapeum
        cl-yaml
        myway
        lack
        lack-middleware-static
        woo
      ];

      nativeBuildInputs = [
        pkgs.makeWrapper
      ];

      installPhase = ''
        runHook preInstall
        mkdir -p $out/bin
        cp -r * $out
        mv $out/nougat-web $out/bin

        wrapProgram $out/bin/nougat-web \
                --prefix LD_LIBRARY_PATH : $LD_LIBRARY_PATH

        runHook postInstall
      '';
    }
  );
in
pkg.overrideAttrs (o: {
  buildScript = pkgs.writeText "build-nougat-web" ''
    (declaim (optimize (speed 3) (space 0) (debug 0)))
    (load "${o.asdfFasl}/asdf.${o.faslExt}")

    (in-package :cl)
       (asdf:load-system :nougat-web)
       (use-package :nougat-web)

      ;; Use uiop:dump-image instead of sb-ext:dump-image for the image restore hooks
      (setf uiop:*image-entry-point* #'nougat-web:main)
      (uiop:dump-image "nougat-web"
                       :executable t
                       #+sb-core-compression :compression
                       #+sb-core-compression t)
  '';

})
