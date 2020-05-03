# spec

## Statement

* `skip` blank statement
* `var := expr` substitute
* `var[expr] := expr` array substitute
* `if expr then statement else statement fi` condition
* `while expr inv expr do statement od` loop
* `statement ; statement` conjunction

## Expression

* `true`, `false`, `1`, `2`... constant value
* アルファベットもしくは`_`で始まり、アルファベット,`_`,数字の羅列 variable
* `v[expr]` select
* `v[expr:=expr]` store
* `+`,`-`,`*`,`div`,`mod` arthmetic (`-` means both of binary and unary)
* `=`,`!=`,`>`,`<`,`>=`,`<=` compare
* `!`,`&`,`|`,`=>` logical
* `ALL x.expr`,`EX x.expr` quantifier


## Operator Precedence

|Precedence|Operator|Associativity|
|:-:|:--|:-:|
|1|`-`(Unary), `!`|-|
|2|`*`, `div`, `mod`|Left-to-Right|
|3|`+`, `-`|Left-to-Right|
|4|`<`, `<=`, `>`, `>=`|Left-to-Right|
|5|`=`, `!=`|Left-to-Right|
|6|`&`, `|`|Left-to-Right|
|7|`=>`|Right-to-Left|
