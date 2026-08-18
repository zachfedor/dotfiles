{
  description = "zachfedor's dotfiles using home-manager on nixos and macos/nix-darwin";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    # Rolling channel, used to cherry-pick a single package that stable freezes
    # too far behind (issue 17): pi-coding-agent. Stable nixos-26.05 pins it at
    # 0.75.4 (pre-Gondolin); unstable tracks upstream's ~weekly bumps (0.84.x),
    # which ship the Gondolin micro-VM sandbox extension. Scoped to that one
    # package via an overlay on athena — the rest of the system stays on stable.
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, nix-darwin, home-manager }:
  let
    user = "zach";

    # Cherry-pick pi-coding-agent (Gondolin sandbox) from unstable; see the
    # nixpkgs-unstable input note above (issue 17). Scoped to athena below.
    piUnstableOverlay = final: prev: {
      pi-coding-agent =
        nixpkgs-unstable.legacyPackages.${prev.stdenv.hostPlatform.system}.pi-coding-agent;
    };

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
        { nixpkgs.overlays = [ piUnstableOverlay ]; }
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
