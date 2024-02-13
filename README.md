# haskell-rust-ffi-template

A template haskell project demonstrating how to use a rust library as a dependency. It is autowired with Nix to provide an environment within which you can work with both haskell and rust.

The development environment also comes with LSP ([rust-analyzer](https://github.com/rust-lang/rust-analyzer) and [haskell-language-server](https://github.com/haskell/haskell-language-server)) support. If you use VSCode, you can use the builtin configuration to get IDE experience without any manual setup (just [install direnv](https://nixos.asia/en/direnv), open in VSCode and accept the suggestions).

## Getting Started

TODO

## Development

```bash
# Dev shell
nix develop
```

We also provide a [`justfile`](https://just.systems/) for Makefile'esque commands.

## Discussion

- [Zulip](https://nixos.zulipchat.com/#narrow/stream/413950-nix)
