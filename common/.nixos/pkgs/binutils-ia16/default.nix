{ lib
, stdenv
, fetchFromGitHub
, bison
, flex
, glibc
, m4
, perl
, pkgconfig
, texinfo
, zlib
}:

stdenv.mkDerivation rec {
  pname = "binutils-ia16";
  version = "20230531";

  src = fetchFromGitHub {
    owner = "tkchia";
    repo = pname;
    rev = "69e12ea2c125ff830580abbe4699d35ba002148b";
    hash = "sha256-7x+yifndYXUIyyr27XmIZgX/pUCzKVW1bWCcmb80Qec=";
  };

  nativeBuildInputs = [
    bison
    flex
    glibc
    m4
    perl
    pkgconfig
    texinfo
    zlib
  ];

  configureFlags = [
    "--prefix=$(out)"
    "--target=ia16-elf"
    #"--bindir=$(out)/bin"
    #"--libdir=$(out)/lib/ia16-elf"
    "--disable-gdb"
    "--disable-libdecnumber"
    "--disable-readline"
    "--disable-sim"
    "--disable-nls"
    "--disable-werror"
    #"--enable-x86-hpa-segelf=yes"
    #"--enable-id=default"
    #"--enable-gold=yes"
  ];

  enableParallelBuilding = true;

  meta = with lib; {
    description = "IA-16 (Intel 16-bit x86) port of GNU Binutils";
    homepage = "https://github.com/tkchia/binutils-ia16";
    license = licenses.gpl2Plus;
    platforms = platforms.all;
    maintainers = with maintainers; [ ];
  };
}
