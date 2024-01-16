__TABLE OF CONTENTS__
<!-- TOC -->
* [ABOUT](#about)
* [SYNTAX](#syntax)
  * [EXPRESSIONS](#expressions)
    * [UNAPPLIED STATE](#unapplied-state)
    * [VARIABLES AND TYPES](#variables-and-types)
    * [PRINT](#print)
<!-- TOC -->

# ABOUT

Languria is a toy programming language made for self-teaching and personal use, written by referencing the excellent [crafting intepreters](https://craftinginterpreters.com/) book by R. Nystrom.

The name (a homophone for "the watermelon" in italian) is the first thing that came to mind that starts with lang-. After having looked the name up, I learned that Languria is also a [nice looking beetle](https://en.wikipedia.org/wiki/Languria), which easily became the language's mascot.

# SYNTAX
## EXPRESSIONS

An expression is a piece of code that can be evaluated, producing a value.
  
    1 // values themselves are expressions.
    (2 + 3) * (4 + 5)  // both (2 + 3) and (4 + 5) are expressions. Of course, the entire line is also an expression.

A scope, which is a section of code between {}, is an expression as well. The value produced by a scope is either the value that was explicitly returned in the middle of it, or what's produced by the last expression in the scope.
    
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

### UNAPPLIED STATE

An expression is in an unapplied state when it contains one of the builtin variables `it`, `ti` or `idx`.

    add_one = it + 1

In order to evaluate an unapplied expression, they need to be fed a value through the application operator `@`.

    2 @ add_one == 3
    add_two = add_one @ add_one  // note that an application whose left hand side is unapplied is, in turn, unapplied.
    2 @ add_two == 4
    2 @ add_one @ add_one == 4  // you can also chain applications directly.
    

### VARIABLES

A variable is a container for an expression.

    x = 1  // variables cannot be declared without being initialized.
    x => 2  // equivalent to x = max(x, 2)
    z =< 1  // equivalent to x = min(x, 1)


### PRINT

You can print an expression by placing an `$` before it.

    $"Hello, world!"
    $x = 1 + 2 // an assignment is an expression that evaluates to the assigned value, so it can be printed.
    y = 2 * $(3 + 4)  // you can also print a subexpression in the middle of a bigger expression.

Multiple `$` on the same line are printed separated by a comma, in the order in which the program executes. This is, in some cases, not intuitive.

    $math = $(3*3) + $(4*4)  
    // clearly, the assignment is the last thing to be evaluated, so the last thing fed to STDIN will be 25.
    // However there's no good reason to expect that this whole line will print "9 16 25" as opposed to "16 9 25".

To remedy this, you can also tag any of your print statements as such.

    $math = $<a>(3*3) + $<b>(4*4)  // prints "a: 9 b: 16 25"

`$$` prints the current location of the line.

    $$ $3  // [test.lgr:4] 3

## TYPES

### ASSOCIATION

An association is a mapping from certain values of a type (keys) to certain values of another type (values). The keys are ordered and cannot repeat.

    int_to_name = [1: "one", 2: "three", _: "not saying"]
    int_to_name #2 == "three"  // you can query an association with the operator #.
    int_to_name #100 == "not saying" // the special _ key associates any unmapped "something" to the value mapped by _. It's always the last key.
    