# haskell-rust-ffi-template

A template haskell project demonstrating how to use a rust library as a dependency. It is integrated with Nix to provide an environment that enables you to:

- Simultaneously develop in Haskell and Rust.
- Use LSP ([rust-analyzer](https://github.com/rust-lang/rust-analyzer) and [haskell-language-server](https://github.com/haskell/haskell-language-server)) for both languages.
- Have builtin support for VSCode (just [install direnv](https://nixos.asia/en/direnv), open in VSCode and accept the suggestions).

## Getting Started

Install Nix, enable Flakes, open in VSCode and run `just run`

See also, <https://nixos.asia/en/haskell-rust-ffi>, if you are into tutorials.

## Development

```bash
# Dev shell
nix develop
```

Or install [direnv + nix-direnv](https://nixos.asia/en/direnv) and enter the shell on `cd`.

We also provide a [`justfile`](https://just.systems/) for Makefile'esque commands.

## Credits

This template combines two other templates, with additional tweaks applied on top:

- [rust-nix-template](https://github.com/srid/rust-nix-template)
- [haskell-template](https://github.com/srid/haskell-template)

## Discussion

- [Zulip](https://nixos.zulipchat.com/#narrow/stream/413950-nix)
