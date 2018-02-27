#ifndef SYS_H
#define SYS_H

// Popcorn library module that depends on the operating system
// Default to Windows, for *nix, just define SYS_UNIX

// Stuff from Ocaml's Filename module is in here too for now.

// This should be fleshed out as needed

// #define SYS_UNIX

prefix Sys {

#ifdef SYS_UNIX

#else

#define SYS_WIN32

#endif

extern string os_type;
extern string current_dir_name;

}
#endif
