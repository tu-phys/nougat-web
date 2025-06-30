{
  description = "A template that shows all standard flake outputs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs, ... }@inputs:
    let
      forAllSys = nixpkgs.lib.genAttrs nixpkgs.lib.platforms.all;
    in
    {
      # Utilized by `nix build`
      packages = forAllSys (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          package = pkgs.callPackage ./default.nix { };
        in
        {
          default = package;
        }
      );

      apps = forAllSys (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/nougat-web";
        };
      });

      # Default module, for use in dependent flakes. Deprecated, use nixosModules.default instead.
      nixosModules.default =
        {
          config,
          lib,
          pkgs,
          ...
        }:
        with lib;
        let
          cfg = config.services.nougat-web;
        in
        {
          options = {
            services.nougat-web = {
              enable = mkEnableOption "Enable nougat web";
              config = mkOption {
                description = "The config.";
                type = types.path;
              };
              package = mkOption {
                type = types.package;
                default = self.packages.${pkgs.system}.default;
                description = "package to use for this service (defaults to the one in the flake)";
              };
            };
          };

          config = mkIf cfg.enable {
            systemd.services.nougat-web = {
              description = "Nougat web";
              wantedBy = [ "multi-user.target" ];

              environment = {
                APP_ENV = "production";
                CONFIG = "${cfg.config}";
              };

              serviceConfig = {
                DynamicUser = "yes";
                ExecStart = "${getExe cfg.package}";
                Restart = "on-failure";
                RestartSec = "5s";
              };
            };
          };
        };
    };
}
