{
  description = "zachfedor's dotfiles using home-manager on nixos and macos/nix-darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
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
    # --- HESTIA (macOS/nix-darwin laptop) ---
    darwinConfigurations.hestia = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit user; };
      modules = [
        ./hosts/hestia/default.nix
        home-manager.darwinModules.home-manager
        hmModule
      ];
    };

    # --- ATHENA (nixOS desktop) ---
    nixosConfigurations.athena = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/athena/default.nix
        home-manager.nixosModules.home-manager
        hmModule
      ];
    };

    # --- ARGUS (Raspberry Pi 3B+ network node; issue 13 slice 2) ---
    # aarch64 despite the Pi's old 32-bit Raspbian (see hosts/argus/default.nix).
    # Build the flashable SD image with:
    #   nix build .#nixosConfigurations.argus.config.system.build.sdImage
    nixosConfigurations.argus = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = [
        ./hosts/argus/default.nix
        home-manager.nixosModules.home-manager
        hmModule
      ];
    };
  };
}
