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

# Demo

Write variable declarations, preconditions, algorithm and postconditions.
If you use loop, you must write loop invariant conditions.

`isqrt.phl`
```
vars x a b m	
{	
  x >= 0	
}	
a := 0;	
b := x + 1;	
while !(a + 1 == b)	
inv a + 1 <= b & a * a <= x & x < b * b	
do	
  m := (a + b) div 2;	
  if m * m <= x then	
    a := m	
  else	
    b := m	
  fi	
od	
{	
  a * a <= x &	
  x < (a + 1) * (a + 1)	
}
```

Execute prohl with phl file above.

```
prohl isqrt.phl
```

Then results will be displayed.

```
 = = = = = = = = = = =

((x >= 0) => ((((0 + 1) <= (x + 1)) & ((0 * 0) <= x)) & (x < ((x + 1) * (x + 1)))))
===> OK

 = = = = = = = = = = =

((!((a + 1) == b) & ((((a + 1) <= b) & ((a * a) <= x)) & (x < (b * b)))) => ((((((a + b) div 2) * ((a + b) div 2)) <= x) => ((((((a + b) div 2) + 1) <= b) & ((((a + b) div 2) * ((a + b) div 2)) <= x)) & (x < (b * b)))) & (!((((a + b) div 2) * ((a + b) div 2)) <= x) => ((((a + 1) <= ((a + b) div 2)) & ((a * a) <= x)) & (x < (((a + b) div 2) * ((a + b) div 2)))))))
===> OK

 = = = = = = = = = = =

((!!((a + 1) == b) & ((((a + 1) <= b) & ((a * a) <= x)) & (x < (b * b)))) => (((a * a) <= x) & (x < ((a + 1) * (a + 1)))))
===> OK
```

If the algorithm satisfies the conditions, all result wil be OK.

If the algorithm does not satisfy the conditions, prohl will show a counterexample.

# How to write phl file

T.B.D.
