__TABLE OF CONTENTS__
<!-- TOC -->
* [ABOUT](#about)
* [SYNTAX](#syntax)
  * [EXPRESSIONS](#expressions)
    * [VARIABLES](#variables)
    * [PRINT](#print)
  * [TYPES](#types)
    * [ASSOCIATION](#association)
  * [THE UNAPPLIED STATE](#the-unapplied-state)
<!-- TOC -->

# ABOUT

Languria is a toy programming language made for self-teaching and personal use, written by referencing the excellent [crafting intepreters](https://craftinginterpreters.com/) book by R. Nystrom.

The name (a homophone for "the watermelon" in italian) is the first thing that came to mind that starts with lang-. After having looked the name up, I learned that Languria is also a [nice looking beetle](https://en.wikipedia.org/wiki/Languria), which easily became the language's mascot.

# SYNTAX
## EXPRESSIONS

An expression is either a primitive value, or any section of the code that evaluates to a primitive value. In languria, everything is an expression.
  
    1 // values themselves are expressions.
    (2 + 3) * (4 + 5)  // both (2 + 3) and (4 + 5) are expressions. Of course, the entire line is also an expression.

A scope, which is a section of code between {}, evalutates to whatever is explicitly returned in the middle of it, or what's produced by the last expression in the scope.

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

### VARIABLES

A variable is a container for an expression.

    x = 1  // variables cannot be declared without being initialized.
    1 == (x = 1)  // an assigment expression returns the assigned value.
    x => 2  // equivalent to x = max(x, 2)
    x =< 1  // equivalent to x = min(x, 1)
    // other assign operators: =*, =/, =%, =^


### PRINT

You can print an expression by placing an `$` before it. The `$expr` expression evaluates to `expr`.

    $"Hello, world!"
    $x = 1 + 2 // an assignment is an expression that evaluates to the assigned value, so it can be printed.
    y = 2 * $(3 + 4)  // you can also print a subexpression in the middle of a bigger expression.

Multiple `$` on the same line are printed separated by a comma, in the order in which the program executes. This is, in some cases, not intuitive.

    $math = $(3 * 3) + $(4 * 4)  
    // clearly, the assignment is the last thing to be evaluated, so the last thing fed to STDIN will be 25.
    // However there's no good reason to expect that this whole line will print "9 16 25" as opposed to "16 9 25".

To remedy this, you can also tag any of your print statements as such.

    $math = $<a>(3 * 3) + $<b>(4 * 4)  // prints "a: 9 b: 16 25"

`$$` prints the current location of the line.

    $$ $3  // [test.lgr:4] 3

## TYPES

### ASSOCIATION

An association is a mapping from certain values of a type (keys) to certain values of another type (values). The keys are ordered and cannot repeat.
You can query (ask for the value corresponding to a key) an association using the operators `#` or `##`. 
The first wraps the result into an option, while the second gives you back the key itself. If the key can't be found, you get an empty wrapping in the first case and an error in the second.

    $int_to_name = [1: "one", 2: "three", _: "not saying"]
    (int_to_name ##2) == "three"  // you can query an association with the operators ## and .
    (int_to_name ##100) == "not saying" // the special _ key associates any unmapped "something" to the value mapped by _. It's always the last key.

If you're following along and trying the commands out, you probably noticed something strange the first line of the above example: 
its REPL output was `[1: (not yet evaluated), 2: (not yet evaluated), _: (not yet evaluated)]`.
This happens because, by default, values in associations are lazily evaluated, in order to prevent side effects and use them for control flow.
In the case above, being queried by `2` caused the key `"three"` to evaluate to that string.
When needed, you can prepend `!!` to an association to evaluate its values right away.
  
    !![3: $"now", 1: $"printing", 2: $"everything]

## THE UNAPPLIED STATE

An expression is in an unapplied state when it contains one of the builtin variables `it`, `ti` or `idx`. Such expressions are called applicables.

    add_one = it + 1

In order to evaluate an applicable, they need to be fed a value through the application operators `@` or `@@`.

    2 @ add_one == 3
    add_two = add_one @ add_one  // note that an application whose left hand side is unapplied is, in turn, unapplied.
    2 @ add_two == 4
    2 @ add_one @ add_one == 4  // you can also chain applications directly.

The `@@` operator iteratively applies an association-style value to the applicable. In such applicaitons, `it` and `ti` carry the key/value pair in the current iteration, while `idx` holds the number of past iterations.
The value produced by such an application is the one produced by the last expression in the last iteration.

    assoc = [1: 2, 3: 4]
    assoc @@ $"\tposition n°{idx} of association is the pair ({it}, {ti})\n"


When you store an unapplied expression into a variable, you can use that variable in the expression's body.

    countdown = [
      it > 1: $(it - 1) @ countdown,
      _: 0
    ] ##true
    
    5 @ countdown  // 4 3 2 1 0