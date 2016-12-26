# Tokamak
Advanced Fusion Reactor

# Tokamak DSL
Tokamak DSL is a Domain Specific Language for .NET, written with *FParsec* parser combinator library.

## Purpose
Tokamak can be embedded in .NET / mono applications (using .NET framework ~4.0+).

## Evaluation
Expressions are evaluated right to left, with no operator precedence. Parentheses are used to evaluate subexpressions before the
outer expression.

## DataTypes
* Integer - corresponds to int64
* Floating - corresponds to double
* Literal - corresponds to string
* Bool - bool
* Array - This is an array of expressions, which may be datatypes or other expressions that can be evaluated to a datatype.

## Functions
Functions are declared using ```function``` keyword followed by the function name, the parameters, the function body, and is
terminated using the ```end``` keyword.

### example
```
function fib(n)
  if n <= 1 then 1
  else
    n * fib(n-1)
  end
end

```

## Array creation
Array declarations are simple:
```
myArray = 0$10 
```
This will create an array [0,1,...,9].
The contents of the array can be modified arbitrarily:

```
myArray[0] = 5.5

```

## Controls
Tokamak has *while-loops* and *for-loops*, as well as *if-elif-else* blocks.

### while loops

```
a = 0
ctr = "a"
while a < 10 do 
  a = a + 1
  ctr = ctr+"a" 
end
```
will produce the string ```aaaaaaaaaa```.

### for loops
```
function iseven(n) (n%2) = 0 end

arr = 0$100

for i in 0$100 do
  arr[i] = iseven(i)
end

arr
```
