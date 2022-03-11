# effect-systems

This repository contains collections of code snippets related to my
[Effect Systems](https://jrpotter.github.io/posts/effect-systems/) blog post.
The post is not yet publically available.

> As I’ve begun exploring the world of so-called algebraic effect systems, I’ve
become increasingly frustrated in the level of documentation around them.
Learning to use them (and moreso, understanding how they work) requires diving
into the internals, watching various videos, and hoping to grok why certain
effects aren’t being interpreted they way you might have hoped. My goal in this
post is to address this issue, at least to some degree, in a focused,
pedagogical fashion. In particular, I’ll aim to build up an intution of the
fused-effects, library, chosen because it seems to have the most active
development, the smallest dependency footprint, and minimal type machinery.

## Building

This repository uses Nix for reproducible builds. First
[install Nix](https://nixos.org/download.html) if you do not currently have it
on your system. Afterward, enable [flakes](https://nixos.wiki/wiki/Flakes) by
adding line

```
experimental-features = nix-command flakes
```

to `$HOME/.config/nix/nix.conf`. You may then use `nix build` and `nix develop`.
To makes things easier, we recommend using [Home Manager](https://github.com/nix-community/home-manager)
to install [direnv](https://github.com/direnv/direnv) and [nix-direnv](https://github.com/nix-community/nix-direnv).
Once you run

```bash
$ direnv allow
```

from the root directory, `nix develop` will be automatically invoked each time
a change is detected in `flake.nix` or you return to the directory.

## Formatting

A `pre-commit` file is included in `.githooks` to ensure consistent formatting.
Run the following to configure `git` to using it:

```bash
git config --local core.hooksPath .githooks/
```
