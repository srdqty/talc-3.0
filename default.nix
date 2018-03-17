{ pkgs ? import <nixpkgs> {}
, stdenv ? pkgs.stdenv
, fetchFromGitHub ? pkgs.fetchFromGitHub
, ocaml ? pkgs.ocaml
, ncurses ? pkgs.ncurses
}:

stdenv.mkDerivation rec {
  name = "talc-${version}";
  version = "3.0";

  src = ./.;

  makeFlags = ["NATIVE=X"];

  buildInputs = [
    ocaml
    ncurses.dev
  ];
}