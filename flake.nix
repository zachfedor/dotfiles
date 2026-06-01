{
  description = "zachfedor's dotfiles for nixos and macos (via nix-darwin) with home-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nix-darwin, home-manager }:
  let
    # Per-machine identity. Add more hosts here as they come online.
    host = "hestia";
    user = "zach";
    system = "aarch64-darwin";
  in {
    darwinConfigurations.${host} = nix-darwin.lib.darwinSystem {
      inherit system;
      modules = [
        # --- system (nix-darwin) ---
        ({ pkgs, ... }: {
          nixpkgs.hostPlatform = system;

          # Determinate Nix manages the daemon and nix itself; let it.
          # nix-darwin must NOT also manage nix or the two fight.
          nix.enable = false;

          # Required by nix-darwin for user-scoped options (e.g. home-manager).
          system.primaryUser = user;
          users.users.${user}.home = "/Users/${user}";

          # Baseline; do not bump casually (see nix-darwin docs).
          system.stateVersion = 5;
        })

        # --- user (home-manager as a darwin module) ---
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          # Back up any pre-existing dotfile HM wants to own (e.g. the old
          # install.sh ~/.gitconfig symlink) instead of erroring on collision.
          home-manager.backupFileExtension = "hm-bak";
          home-manager.users.${user} = import ./home.nix;
        }
      ];
    };
  };
}
