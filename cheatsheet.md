# Haskell Cheatsheet

## GHCi commands
- `:load`(`:l`) load source file
- `:cd` change directory
- `:reload`(`:r`) reload current file
- `:type`(`:t`) show type information
- `:module`(`":m"`) load module

## Numeric Types and Type Classes

- `Int`: integer with limited range
- `Integer`: integer with arbitrary precision
- `Float`: single-precision floating point number 
- `Double`: double-precision floating point number

```
    Eq => Num => Fractional
              ╲       ╲
          Ord => Real => Double / Float
                      ╲
                 Enum => Integral => Integer
                                  ╲
                         Bounded  => Int
```
