# prohl
An algorithm verifier based on Hoare Logic.

It was developed with reference to [プログラム検証器を作って学ぶ Hoare 論理](https://principia.connpass.com/event/174880/)

# Dependency

## z3

```
apt install z3
```

# Build

To build prohl, you need to install Haskell Platform.

```
apt install haskell-platform
```

At working directory, run the follows.

```
git clone https://github.com/elkel53930/prohl.git
cd prohl
cabal build
```
This will generate executable binary file in `{WORKING_DIRECTORY}/prohl/dist/build/prohl`.

# Usage

```
prohl XXXX.phl
```

# How to write phl file

T.B.D.
