# Maintainer: Timéo Arnouts <tim.arnouts@protonmail.com>
pkgname=indigobi
pkgver=v0.1.0
pkgrel=1
pkgdesc="A cute Gemini client written in OCaml."
arch=('any')
url="https://github.com/Tim-ats-d/Indigobi"
license=('GPL3')
options=('!strip')
makedepends=('dune' 'ocaml-topkg' 'ocaml-findlib' 'ocaml-compiler-libs>=4.13.0' 'ocamlbuild' 'opam' 'git')
optdepends=('xdg-open: external file opening support on UNIX')
source=("$pkgname-$pkgver.tar.gz::git+https://github.com/Tim-ats-d/Indigobi/archive/refs/tags/$pkgver.tar.gz")
changelog=
sha256sums=('abdc7d26ff4a30d159b869cd03d3e505eb75c757cb7889cfa2c03d28fd71d5ef')

build() {
  cd ${pkgname}-$pkgver
  dune build --profile release
}

package() {
  cd $srcdir/$pkgname

  make install
  install -D -m 644 "LICENSE" "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}

pkgver() {
  cd "${srcdir}/${pkgname}"

  echo "$(git rev-list --count HEAD).$(git rev-parse --short HEAD)"
}
