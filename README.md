# Collage

Create a collage of images
randomly chosen
from given images.
The collage does not necessarily contain *all* images.
If the first chosen image neatly fits the given space,
it will be the only image in the "collage".

Collage was originally designed
to aid random wallpaper selection,
where the set of possible wallpapers
may contain images with aspect ratios
that do not fit the display.
Instead of filling the remaining space
with a solid color,
or a blur effect,
Collage fills the remaining space
with more images.

For example,
`collage -w 1920 -h 1080 ~/pictures/wallpapers/* -t ~/collage.jpg`
may produce

![collage of bunnies](https://user-images.githubusercontent.com/7183441/91667930-3cab9480-ead6-11ea-8404-8093b6445cc4.jpg)

or

![another collage of bunnies](https://user-images.githubusercontent.com/7183441/91667934-433a0c00-ead6-11ea-8473-dcea4b9c2967.jpg)

or

![even more bunnies](https://user-images.githubusercontent.com/7183441/91667935-4c2add80-ead6-11ea-8cc4-28ce04a60303.jpg)

## Installation

### NixOS

Add a Nix overlay like

```
nixpkgs.overlays = [
  (self: super: {
    collage = (import (self.fetchFromGitHub {
      owner = "JustinLovinger";
      repo = "collage";
      rev = "LATEST_VERSION_TAG";
      sha256 = "SHA25_OF_LATEST_VERSION_TAG";
    }) { pkgs = self; });
  })
];
```

Get the `sha256`
with `nix run nixpkgs.nix-prefetch-github -c nix-prefetch-github --rev "LATEST_VERSION_TAG" JustinLovinger collage`.

Add the package like

```
environment.systemPackages = with pkgs; [
  collage
];
```

If Collage does not build with `{ pkgs = self; }`,
you can replace it with `{ }`
to use pinned development dependencies.

### Other Linux

Follow build instructions
and copy files
to the expected paths
for your distribution.

## Usage

See `man collage` or `collage --help`.

## Development

Enter a development shell with `nix-shell`.

## Building

Build the program with `nix-build`.
Build man pages with `nix-build -A man`.
Build everything at once with `nix-build -A all`.
