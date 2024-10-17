# Summary

This is a utility command-line tool to explore changes to GHC by building stackage stapshots and recording build outputs and results of running tests. Successful and failed builds are tracked along with the outputs that tests produced during execution. The intended mode of operation is to collect outputs for two different GHCs and then diff them so see which packages started/stopped building, which tests started/stopped passing and what were the error messages for further exploration.

# How to use

## Dependencies

- Stackage snapshots are built with docker so docker is mandatory.
- GHC
- Cabal

## Setup

Set up docker per your system’s guidelines, then install FPComplete’s docker image for the snapshot you want to build. E.g. for LTS 22 it would be:

```
$ sudo docker run --interactive --tty ghcr.io/commercialhaskell/stackage/build:lts22 bash -l
```

## Enter docker shell

Setting up rootless docker seems to work so using sudo to enter shell is not required

Suppose my project is

```
$ mkdir /tmp/stackage-docker-tmp # Build will dump stuff here for easier removal later, optional mount
$ docker run --interactive --tty --mount 'type=bind,src=$HOST_WORKDIR,$INTERNAL_WORKDIR --mount 'type=bind,src=/tmp/stackage-docker-tmp,destination=/tmp' ghcr.io/commercialhaskell/stackage/build:lts22 bash -l
```

## On the inside

The `##` prompt signifies the prompt you get after entering docker image. It’s supposed to
mean root privileges in the container you’ll get by default.

Install suitable ghc and cabal for the LTS version you want to build, add them to PATH. Then install some of the dependencies that snapshot packages need to run their tests. You may need to install more depending on build failures you observe.

```
## apt update && apt install markdown-unlit npm --yes
```

### Get the snapshot you want to test

```
## wget https://www.stackage.org/lts-22.38/cabal.config -O cabal.config-lts-22.38
```

You may also want to get the cabal flags stackage builds with. Or use these as a starting point:

```
constraints:
  -- , cabal-install -native-dns
  -- , nixpaths +allow-relative-paths

  , QuickCheck -old-random
  , pathtype -old-time
  , bz2 -with-bzlib
  , mersenne-random-pure64 -small_base
  , cloud-haskell +tcp +simplelocalnet +p2p
  , curl +new-base
  , hpio -test-hlint
  , idris +ffi
  , hxt +network-uri
  , hxt-http +network-uri
  , hxt-relaxng +network-uri
  , text -integer-simple
  , tar -old-time
  , time-locale-compat -old-locale
  , HsOpenSSL -fast-bignum
  , cabal-rpm -old-locale
  , NineP -bytestring-in-base
  , nix-paths +allow-relative-paths
  , fay +test
  , reedsolomon -llvm
  , jsaddle +gtk3
  , functor-classes-compat +containers
  , alerta -servant-client-core
  , cabal-install -native-dns
  , greskell -server-test
  , windns +allow-non-windows
  , hsdev -docs
  , bson -_old-network
  , mongoDB -_old-network
  , sdl2 -recent-ish
  , formatting +no-double-conversion
  , hackage-security +Cabal-syntax
  , aws-sns-verify +development
  , optics-operators -build-readme
  , bm +optparse-applicative_ge_0_18
  , horizontal-rule +optparse-applicative_ge_0_18
  , literatex +optparse-applicative_ge_0_18
  , phatsort +optparse-applicative_ge_0_18
  , redact +optparse-applicative_ge_0_18

  , acid-state -skip-state-machine-test

package *
  shared: True
  static: False
  executable-dynamic: True
  executable-static: False
  split-sections: True
```

I’ll put the flags in `cabal.config.lts-flags` file in the examples below.

### Run the utility

NB avoid using `cabal run` because `stackage-tester` also uses `cabal run` and nesting leads to errors.

```
## git clone https://github.com/sergv/stackage-tester.git # not ewhere it goes
## export PATH_TO_STACKAGE_TESTER_REPO=...
## (cabal build --project-dir "$PATH_TO_STACKAGE_TESTER_REPO" stackage-tester)
## $(cabal list-bin --project-dir "$PATH_TO_STACKAGE_TESTER_REPO" stackage-tester) --logs-dir /tmp/stackage-work-logs --cabal-config-main cabal.config-lts-22.38 --extra-cabal-config cabal.config.lts-flags -j 10 --with-ghc=ghc-9.6.6 --stick-to-stackage-packages +RTS -s
```

The tool will complain if it doesn’t find some Haskell executables that are known to be used by some package’s tests so it will check that they’re in the PATH and abort if they’re not. They’re all installable by plain cabal.

That’s probably it for now, if anything is unclear please open issues in this repository.
