__TABLE OF CONTENTS__
<!-- TOC -->
* [ABOUT](#about)
* [EXPRESSIONS](#expressions)
  * [VARIABLES](#variables)
  * [PRINT](#print)
  * [TYPES](#types)
    * [STRINGS](#strings)
    * [OPTIONS](#options)
    * [BOOLEANS](#booleans)
    * [INTEGERS AND FLOATS](#integers-and-floats)
    * [ASSOCIATIONS](#associations)
    * [APPLICABLES](#applicables)
<!-- TOC -->

# ABOUT

Languria is a toy programming language made for self-teaching and personal use, written by referencing the excellent [crafting intepreters](https://craftinginterpreters.com/) book by R. Nystrom.

The name (a homophone for "the watermelon" in italian) is the first thing that came to mind that starts with lang-. After having looked the name up, I learned that Languria is also a [nice looking beetle](https://en.wikipedia.org/wiki/Languria), which easily became the language's mascot.

# EXPRESSIONS

An expression is either a primitive value, or any section of the code that evaluates to a primitive value.
A value is an instance of a primitive type (`int`, `str`, ...). In languria, everything is an expression.
```
1 // values themselves are expressions.
(2 + 3) * (4 + 5)  // both (2 + 3) and (4 + 5) are expressions. Of course, the entire line is also an expression.
```
This is an overview of the primitive types:

| Type        | Example      | Overview                     |
|-------------|--------------|------------------------------|
| Integer     | `1`          | [link](#INTEGERS-AND-FLOATS) |
| Float       | `1.5`        | [link](#INTEGERS-AND-FLOATS) |
| Boolean     | `true`       | [link](#BOOLEANS)            |
| String      | `example`    | [link](#STRINGS)             |
| Association | `[1: "one"]` | [link](#ASSOCIATIONS)        |
| Option      | `<0.8>`      | [link](#OPTIONS)             |
| Applicable  | `it * 2`     | [link](#APPLICABLES)         |
| Literal     | `x`          | -                            |

A scope, which is a section of code between {}, evaluates to whatever is explicitly returned in the middle of it, or what's produced by the last expression in the scope.
```
rps = {
    return "rock"
    "paper"
    "scissor"
}
phrase = {
    "how"
    "are"
    "you"
}  
"{phrase} {rps}" == "you rock"  // strings accept tokens between curly brackets by default.
```
## VARIABLES

A variable is a literal that is holding a value.
```
x = 1  // variables cannot be declared without being initialized.
1 == (x = 1)  // an assigment expression returns the assigned value.
x => 2  // equivalent to x = max(x, 2)
x =< 1  // equivalent to x = min(x, 1)
// other assign operators: =*, =/, =%, =^
```
## PRINT

You can print an expression by placing an `$` before it. The `$expr` expression evaluates to `expr`.
```
$"Hello, world!"
$x = 1 + 2 // an assignment is an expression that evaluates to the assigned value, so it can be printed.
y = 2 * $(3 + 4)  // you can also print a subexpression in the middle of a bigger expression.
```
Multiple `$` on the same line are printed separated by a comma, in the order in which the program executes. This is, in some cases, not intuitive.
In the example below, we can rest assured that the assignment is the last thing to be evaluated, so the last thing fed to STDIN will be 25. However, there's no good reason to expect that this whole line will print "9 16 25" as opposed to "16 9 25".
```
$math = $(3 * 3) + $(4 * 4)
```
To remedy this, you can also tag any of your print statements as such.
```
$math = $<a>(3 * 3) + $<b>(4 * 4)  // prints "a: 9 b: 16 25"
```
`$$` prints the current location of the line. Note that everything is a value, including `$$`!
```
$$ $3  // [test.lgr:4] 3
$$ * 2  // not only this prints [REPL:1], but it also evaluates to [REPL:1][REPL:1]
```

## TYPES
### STRINGS

The following expressions all evaluate to `"Hello, world!"`:
```
"Hello, " + "world!"
"He" + 2*"l" + "o, world!"
{
  greeting = "Hello"
  object = "world"
  "{greeting}, {object}!"
}
```

Note that a single string {placeholder} only accepts one single literal: you cannot evaluate expressions inside a string.

### OPTIONS

An option value is a container in which there could either be another value, or nothing.

```
yes_int = ?2
no = ?_ // the empty option
```

To extract a value from an option that contains one, you can use the `|>` operator. 
This operation fails if there's no value to pull out.

```
?3|> == 3
// ?_|>  // cannot apply `EXTRACT` to operands `[?_]`
```

### BOOLEANS

A boolean value is either `true` or `false`. Booleans can be combined with the logical operations `not`, `and`, `or`, `xor`.

The postfix operator `?!` can be used to interpret a value as a boolean:

```
0?! == false
2.9?! == true  // integers and floats are intepreted as true when strictly greater than 0
""?! == false
[]?! == false
"something"?! == true
[1: 2]?! == true  // strings and associations are true when not empty
?_?! == false
?"something"?! == true // option values evaluate to true if they contain a value
```

### INTEGERS AND FLOATS

Integers and floats are 64bit and signed. Languria implicitly converts between the two if needed be.

The builtin operations are: `+`, `-`, `*`, `%`, `/` (whole division when both operands are ints), `^` (exponentiation, also used for roots if you pass a fractional exponent)
```
17 % 3 == 2
17/3  // 5
17/3  // 5.666666666666667
8 ^ 4  // 4096
8 ^ (1.0/3)  // 2.0
```

### ASSOCIATIONS

An association is a mapping from certain values (keys) to certain values (still called values). The keys are ordered and cannot repeat.

You can pull a value from an association using the operator `>>` pointing to its key. The result of this operation is an option, containing the value you were looking for inside it if the pull was successful.

You can also skip this extra step and get the associated value right away through the `|>>` operator.
```
$int_to_name = [1: "one", 2: "two", _: "not saying"]
(int_to_name >>2) == ?"two"
(int_to_name |>>2) == "two"
(int_to_name >>100) == ?"not saying" // the special _ key associates any unmapped "something" to the value mapped by _. It's always the last key.

no_default = [1: 2]
(no_default >>100) == ?_

[1: 1, "two": 2, 3: ?3]  // neither keys nor values have to share their types.
```
If you're following along and trying the commands out, you probably noticed something strange the first line of the above example:
its REPL output was `[1: (not yet evaluated), 2: (not yet evaluated), _: (not yet evaluated)]`.

This happens because, by default, values in associations are lazily evaluated, in order to prevent side effects and use them for control flow.
In the case above, being queried by `2` caused the key `"three"` to evaluate to that string.

When needed, you can prepend `!!` to an association to evaluate its values right away.
```
!![3: $"now", 1: $"printing", 2: $"everything"]
```

To push a key/value pair into an association, the `<<` operator is used.

```
new_association = [] << |1, 3|  // [1: 3]
new_association << |1, 4|  // [1: 4]  // if the key already exists, its value is overridden.
new_association << |_, 5|  // [1; 4, _: 5]  // you can also set a default value like such.
new_association << |1, _|  // [_: 5]  // pushing an underscore signifies that we want to drop that key.
new_association << |_, _|  // []  // by the same token, you can also drop the association's default value in this way.
```

Associations are, given certain conditions, treated as being in particular states.
An association is in a _set state_ when all of its keys map to booleans, and it's in a _list state_ when its keys are the integers 0 to n.

Lists and sets are easier to both instantiate and operate:
```
//  the :[items] shorthand, when declaring a list, refers to the fact that the right side of the key:value relationships can be inferred.
list = :["faster", "way", "to", "declare", "associations"]
//  a similar reasoning explains the [:items] annotation. here, the values are all set to true.
set = [:"faster", "way", "to", "declare", "associations"]
// remember that keys are unique
!![:1, 2, 3, 2, 1]  // [1: true, 2: true, 3: true]
```

If the side of the key/value pair that we care about is a contiguous range of integers from a to b not including b, we can use this range syntax:
```
same_list = !!:[1..6]
// if the range start is greater than the range's end, the numbers are reversed. 
// note that the range start is still inclusive, and the range still stops before reaching the range's end.
reversed_list = !!:[5..0]
```



### APPLICABLES

An applicable is an expression that holds onto a piece of code with some placeholders, 
and that undergoes an additional evaluation phase, called application, if fed values through the operators `@` or `@@`.

During an application, the applicable's body is evaluated with those values in place of the placeholders.
Below are some basic examples of normal @-applications.
```
add_one = |n| n + 1
|2| @ add_one == 3
|2| @ add_one == 2 @ add_one  // if the applicable needs to be fed only 1 value, the bars are optional.
2 @ add_one @ add_one == 4 // you can chain applicables.
|4| @ |n|(n ^ 2) @ |n|n / 3  // 5
// careful about specifying the applicable's limits! without the parentheses, this is what would've been specified:
|4| @ |n| n^(2 @ |n| n/3)  // 1
```

The operator `@@` is, instead, special in terms of its input, effect, receiver and output.
- The input is an association, or anything that could otherwise be thought of as a collection of elements (currently only strings fit this second category).
- The effect of this application on an expression is to evaluate the expression's body once for every element.
- The body of the applicable, instead of requiring the named placeholders seen above, uses three built-in placeholders:
`it` stores the key of the element, `ti` its value, and `idx` holds the number of past iterations.

The value produced by such an application is the one produced by the last expression in the last iteration.
```
assoc = [1: 2, 3: 4]
assoc @@ ($"\tposition n°{idx} of association is the pair ({it}, {ti})\n")
// the result of this application is the string "position n°1 of association is the pair (3, 4)"

"hello" @@ it  == "o"
"hello" @@ idx == 4
```

Feeding `_` to an @@-applicable will cause the applicable to only stop when it encounters a return value.

```
generate_odd_digits = |amount| {
    result = !!:[1]
    _@@ { result@@idx == amount - 1 and return result
    result << | result @@ idx + 1, result @@ ti + 2|
    }
}

10 @ generate_odd_digits
```

When you store an unapplied expression into a variable, you can use that variable in the expression's body.

```
countdown = |tick| [
  tick > 1: |$(tick - 1)| @ countdown,
  _: 0
] |>> true

5 @ countdown  // prints 4 3 2 1 and returns 0
```

If you wish to define an applicable that accepts more than one value, you can use a more traditional syntax and specify its arguments like such:

```
add = |a, b| a + b
|3, 2| @ add == 5 

ok_or = |opt, default| [opt?!: opt|>, _: default] |>> true
|?3, 4| @ ok_or == 3
|?_, 4| @ ok_or == 4

map_val = |assoc, fn_val| {
    new_assoc = []
    assoc @@ {
        new_assoc << |ti, it @ fn_val|
    }
    new_assoc
}

|:[1..6], (|n|n^2)| @ map_val  // [1: 0, 2: 1, 3: 4, 4: 9, 5: 16]
```