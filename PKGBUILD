# Maintainer: Timéo Arnouts <tim.arnouts@protonmail.com>
pkgname=indigobi
pkgver=96.ae67376
pkgrel=1
pkgdesc="A cute Gemini client written in OCaml."
arch=('any')
url="https://github.com/Tim-ats-d/Indigobi"
license=('GPL3')
depends=('opam')
makedepends=('git')
optdepends=('xdg-open: external file opening support on UNIX')
install=
source=("$pkgname::git+https://github.com/Tim-ats-d/Indigobi")
changelog=
md5sums=('SKIP')

build() {
  cd $srcdir/$pkgname

  opam install --deps-only .
  make build TARGET=UNIX
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
