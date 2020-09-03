# Haskell Cheatsheet

## GHCi commands
- `:load`(`:l`) load source file
- `:cd` change directory
- `:reload`(`:r`) reload current file
- `:type`(`:t`) show type information
- `:module`(`":m"`) load module
- `:browse` types for functions from a module

## Numeric Types and Type Classes

- `Int`: integer with limited range
- `Integer`: integer with arbitrary precision
- `Float`: single-precision floating point number 
- `Double`: double-precision floating point number

```
          Num => Fractional => Floating(Complex)
              ╲            ╲            ╲
    Eq => Ord => Real => RealFrac => RealFloat(Double / Float)
                      ╲
                 Enum => Integral(Integer, Int)

                         Bounded(Int)
```

## Defining and Importing Modules

``` haskell
module MyModule where 
    -- basic module definition
module MyModule (myFunc1) where
    -- only export myFunc1
module MyModule (MyData(MyConstructor)) 
    -- only export specific constructor of a data type

import MyModule 
    -- import MyModule
import MyModule (myFunc1, myFunc2)
    -- import 2 functions from module
import MyModule (MyData(MyConstructor))
    -- import a constructor of data type from module
import qualified MyModule
    -- qualified import, use MyModule. prefix before functions
import MyModule hiding (hiddenFunc) 
    -- import module without hiddenFunc, algebraic datatypes and type synonyms cannot be hidden
import MyModule as MM 
    -- rename module, can rename different module to same name

```
