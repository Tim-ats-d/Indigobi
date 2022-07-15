# Maintainer: Timéo Arnouts <tim.arnouts@protonmail.com>
pkgname=indigobi
pkgver=87.4c3c6f6
pkgrel=1
pkgdesc="A cute Gemini client written in OCaml."
arch=('any')
url="https://github.com/Tim-ats-d/Indigobi"
license=('GPL3')
depends=('dune'
         'opam')
makedepends=('git')
optdepends=('xdg-open: external file opening support on UNIX')
install=
changelog=
source=("$pkgname::git+https://github.com/Tim-ats-d/Indigobi")
md5sums=('SKIP')

build() {
  cd $srcdir/$pkgname

  echo $srcdir

  opam install --deps-only .
  make build-unix
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
