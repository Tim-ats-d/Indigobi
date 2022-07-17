# Indigobi

Indigobi is a WIP **Gemini client** written in OCaml.

<p align="center">
  <img src="assets/demo.png" alt="Demo">
</p>

## Features

- Pretty gemini pages rendering.
- External file in their default app opening.
- Browsable history.

#### Not yet implemented

- A TUI with `lambda-term`.
- Lang and charset attributes in MIME section of header handling.

## Installing

### By hand

```
$ make deps
$ make build TARGET=UNIX
# or make build TARGET=DARWIN according to your system.
$ make install
```

### Using makepkg

```
$ makepkg
```

It assumes you have an opam switch setup for OCaml 4.13. If not:

```
$ opam init
$ opam switch create indigobi 4.13.0
$ eval $(opam env)
```

## Contributing

Pull requests, bug reports, and feature requests are welcome.

## License

- **GPL 3.0** or later. See [license](LICENSE) for more information.
