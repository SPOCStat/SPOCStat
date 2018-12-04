<div align="center">
<img alt="SPOCS logo" src="./assets/SPOCS_logo.svg" width="35%" height="35%"/>
</div>

# SPOCS :vulcan_salute:

[![Build Status](https://travis-ci.com/SPOCStat/SPOCStat.svg?branch=master)](https://travis-ci.com/SPOCStat/SPOCStat)

Live long and prosper: use SPOCS!

__SPOCS__ is a software library written in modern Fortran for
efficiently computing descriptive statistical moments and
co-moments. The [SPOCS acronym] stands for:

* [__S__ ingle-point]
* [__P__ arallel]
* [__O__ nline]
* [__C__ onverging]
* [__S__ tatistics]

## Features

- [x] Written in Modern Fortran
- [ ] C bindings
- [ ] Python bindings
- [ ] Modern CMake based build system
- [ ] Test suite and modern development practices including continuous
      integration testing
- [ ] Supports all floating point formats supported by Fortran
      compiler & host
- [ ] API Documentation using [FORD]
- [ ] Examples and futher usage information on the wiki
- [ ] Coarray based API for parallel processing
- [ ] MPI based API for parallel processing
- [ ] OpenMP based API for parallel processing
- [x] Large integer support for processing more elements than can be
      represented by a 4-byte integer
- [x] Reduced numerical error relative to most other one-pass
      algorithms, and even two-pass traditional algorithms
- [x] Ability to watch and quantify level of statistical convergence,
      useful for estimating rate of convergence, error and/or
      uncertainty in quantities of interest, and to stop processing
      data once the desired convergence level is reached
- [x] Abilty to monitor and stream the statistics as data is streamed
      to be processed (Online processing andm monitoring)
- [x] Ability to partition data into subsets and process those
      asynchronously and/or simultaneously--the final result can be
      obtained with a simple reduction operation
- [x] Finite, small state size in memory, dependent only on the number
      and type of statistics collected, *NOT* the number of points
      visited

## System requirements

- Linux or macOS, will likely work on other unix based systems
- Untested on Windows, YMMV
- CMake 3.12 or later, binaries are available on the CMake website and
  are easily installed into user-space for macOS, Linux and Windows
- A recent Fortran compiler with good Fortran 2003 support and some
  Fortran 2008 support
  - Tested with GFortran 8.2
  - Tested with Intel 18.3

## Building SPOCS

Follow the usual simple CMake based installation workflow:

``` bash
mkdir build
cd build
export FC=/path/to/modern/Fortran/compiler
export CC=/path/to/C/compiler
cmake -DCMAKE_BUILD_TYPE:STRING=RELEASE -DCMAKE_INSTALL_PREFIX=/usr/local ..
cmake --build . -- -j $(nproc) # or just make -j
ctest --output-on-failure # Run tests to ensure proper functionality
sudo cmake --build . --target install # or just sudo make install
# sudo only required if you're installing into a system directory that needs root privs
```

---

<div align="center">

Made with <3 by [@zbeekman]

</div>

[top]: https://github.com/SPOCStat/SPOCStat#readme
[SPOCS acronym]: ./SPOCS-definition.md
[__S__ ingle-point]: ./SPOCS-definition.md#single-point
[__P__ arallel]: ./SPOCS-definition.md#parallel
[__O__ nline]: ./SPOCS-definition.md#online
[__C__ onverging]: ./SPOCS-definition.md#converging
[__S__ tatistics]: ./SPOCS-definition.md#statistics
[@zbeekman]: https://github.com/zbeekman
