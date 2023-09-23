{ lib
, stdenv
, fetchFromGitHub
, binutils-ia16
, flex
, gmp
, libmpc
, mpfr
, pkgconfig
, zlib
}:

stdenv.mkDerivation rec {
  pname = "gcc-ia16";
  version = "20230704";

  src = fetchFromGitHub {
    owner = "tkchia";
    repo = pname;
    rev = "a4c99e280eca6188ef2a69fcabe5af9f14d7eeb6";
    hash = "sha256-9G/X0sksJH0lrI1F7Hy1O21Gh+6XUS7cxu8QmqO0KvU=";
  };

  nativeBuildInputs = [
    #bison
    flex
    gmp
    libmpc
    #m4
    mpfr
    #perl
    pkgconfig
    #texinfo
    zlib
  ];

  buildInputs = [
    binutils-ia16
  ];

  preConfigure = ''
    mkdir ../build
    cd ../build
    configureScript=../$sourceRoot/configure
  '';

  configureFlags = [
    "--prefix=$(out)"
    "--target=ia16-elf"
    #"--bindir=$(out)/bin"
    #"--libdir=$(out)/lib/ia16-elf"
    "--enable-languages=c"
    "--disable-multilib"
    "--disable-nls"
    "--disable-headers"
    "--disable-libssp"
    "--disable-isl"
    "--disable-werror"
  ];

  #postInstall = ''
  #  cd ..
  #  rm -rf gcc-ia16-build
  #'';

  enableParallelBuilding = true;

  meta = with lib; {
    description = "IA-16 (Intel 16-bit x86) port of GNU GCC";
    homepage = "https://github.com/tkchia/gcc-ia16";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
