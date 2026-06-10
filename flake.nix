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
    user = "zach";

    # Shared home-manager wiring, identical across hosts. The platform module
    # (darwinModules / nixosModules) is supplied per-host below. The user config
    # itself (./home.nix) is cross-platform with inline isDarwin/isLinux guards.
    hmModule = {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      # Back up any pre-existing dotfile HM wants to own instead of erroring on
      # collision (e.g. the old install.sh ~/.gitconfig symlink).
      home-manager.backupFileExtension = "hm-bak";
      home-manager.users.${user} = import ./home.nix;
    };
  in {
    # --- macOS desktop (nix-darwin) ---
    darwinConfigurations.hestia = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit user; system = "aarch64-darwin"; };
      modules = [
        ./hosts/hestia/default.nix
        home-manager.darwinModules.home-manager
        hmModule
      ];
    };

    # --- NixOS desktop (athena) is added in issue 05b ---
    # nixosConfigurations.athena = nixpkgs.lib.nixosSystem { ... };
  };
}
