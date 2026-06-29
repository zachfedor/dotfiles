# colima + docker CLI instead of Docker Desktop

Containers on macOS run via `colima` (a headless lima-based Linux VM) with the
`docker`/`docker-compose` CLIs from nixpkgs, rather than Docker Desktop. Chosen to
avoid Docker Desktop's commercial-licensing strings and GUI weight, and to keep
the `docker` command identical across hosts — on athena (NixOS) the same CLI talks
to a native `virtualisation.docker.enable` daemon, no VM needed. The trade-off is
no Docker Desktop dashboard/Kubernetes one-click and a manual `colima start`;
acceptable since per-project services come from devShell/`docker-compose` anyway
(ADR-0004).
