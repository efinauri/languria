__TABLE OF CONTENTS__
<!-- TOC -->
* [ABOUT](#about)
* [EXPRESSIONS](#expressions)
  * [VARIABLES](#variables)
  * [PRINT](#print)
  * [TYPES](#types)
    * [STRINGS](#strings)
    * [BOOLEANS](#booleans)
    * [INTEGERS AND FLOATS](#integers-and-floats)
    * [ASSOCIATIONS](#associations)
    * [APPLICABLES](#applicables)
    * [OPTIONALS](#optionals)
<!-- TOC -->

# ABOUT

Languria is a toy programming language made for self-teaching and personal use, written by referencing the excellent [crafting intepreters](https://craftinginterpreters.com/) book by R. Nystrom.

The name (a homophone for "the watermelon" in italian) is the first thing that came to mind that starts with lang-. After having looked the name up, I learned that Languria is also a [nice looking beetle](https://en.wikipedia.org/wiki/Languria), which easily became the language's mascot.

# EXPRESSIONS

An expression is either a primitive value, or any section of the code that evaluates to a primitive value.
A value is an instance of a primitive type (`int`, `str`, ...). In languria, everything is an expression.
  
    1 // values themselves are expressions.
    (2 + 3) * (4 + 5)  // both (2 + 3) and (4 + 5) are expressions. Of course, the entire line is also an expression.

This is an overview of the primitive types:

| Type        | Example      | Overview                     |
|-------------|--------------|------------------------------|
| Integer     | `1`          | [link](#INTEGERS-AND-FLOATS) |
| Float       | `1.5`        | [link](#INTEGERS-AND-FLOATS) |
| Boolean     | `true`       | [link](#BOOLEANS)            |
| String      | `example`    | [link](#STRINGS)             |
| Association | `[1: "one"]` | [link](#ASSOCIATIONS)        |
| Optional    | `<0.8>`      | [link](#OPTIONALS)           |
| Applicable  | `it * 2`     | [link](#APPLICABLES)         |
| Literal     | `x`          | -                            |

A scope, which is a section of code between {}, evaluates to whatever is explicitly returned in the middle of it, or what's produced by the last expression in the scope.

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

## VARIABLES

A variable is a literal that is holding a value.

    x = 1  // variables cannot be declared without being initialized.
    1 == (x = 1)  // an assigment expression returns the assigned value.
    x => 2  // equivalent to x = max(x, 2)
    x =< 1  // equivalent to x = min(x, 1)
    // other assign operators: =*, =/, =%, =^

## PRINT

You can print an expression by placing an `$` before it. The `$expr` expression evaluates to `expr`.

    $"Hello, world!"
    $x = 1 + 2 // an assignment is an expression that evaluates to the assigned value, so it can be printed.
    y = 2 * $(3 + 4)  // you can also print a subexpression in the middle of a bigger expression.

Multiple `$` on the same line are printed separated by a comma, in the order in which the program executes. This is, in some cases, not intuitive.
In the example below, we can rest assured that the assignment is the last thing to be evaluated, so the last thing fed to STDIN will be 25. However, there's no good reason to expect that this whole line will print "9 16 25" as opposed to "16 9 25".

    $math = $(3 * 3) + $(4 * 4)

To remedy this, you can also tag any of your print statements as such.

    $math = $<a>(3 * 3) + $<b>(4 * 4)  // prints "a: 9 b: 16 25"

`$$` prints the current location of the line.

    $$ $3  // [test.lgr:4] 3


## TYPES
### STRINGS

The following expressions all evaluate to `"Hello, world!"`:

    "Hello, " + "world!"
    "He" + 2*"l" + "o, world!"
    {
      greeting = "Hello"
      object = "world"
    "{greeting}, {object}!"
    }

Note that a single string {placeholder} only accepts one single literal: you cannot evaluate expressions inside a string.

### BOOLEANS

### INTEGERS AND FLOATS

Integers and floats are 64bit and signed. Languria implicitly converts between the two if needed be.

The builtin operations are: `+`, `-`, `*`, `%`, `/` (whole division when both operands are ints), `^` (exponentiation, also used for roots if you pass a fractional exponent)

    17 % 3 == 2
    17/3  // 5
    17/3  // 5.666666666666667
    8 ^ 4  // 4096
    8 ^ (1.0/3)  // 2.0

### ASSOCIATIONS

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

### APPLICABLES

An applicable is an expression that holds onto a piece of code with some placeholders, and that undergoes an additional evaluation phase, called application, if fed values through the operators `@` or `@@`.
During an application, the applicable's body is evaluated with those values in place of the placeholders.

    add_one = it + 1
    2 @ add_one == 3
    add_two = add_one @ add_one  // note that an application whose left hand side is unapplied is, in turn, unapplied.
    2 @ add_two == 4
    2 @ add_one @ add_one == 4  // you can also chain applications directly.

Applicables have 3 default placeholders: `it`, `ti` and `idx`. During an application in which a single value fed to the applicable, `it` is the only placeholder needed.

The situation where `ti` and `idx` play a role is a @@-application. Here, an association-style value is iteratively fed to the applicable, one key/value pair at a time.
For each iteration, `it`/`ti` carry, respectively, the key/value, and `idx` holds the number of past iterations.

The value produced by such an application is the one produced by the last expression in the last iteration.

    assoc = [1: 2, 3: 4]
    assoc @@ $"\tposition n°{idx} of association is the pair ({it}, {ti})\n"
    // the result of this application is the string "position n°1 of association is the pair (3, 4)"

When you store an unapplied expression into a variable, you can use that variable in the expression's body.

    countdown = [
      it > 1: $(it - 1) @ countdown,
      _: 0
    ] ##true
    
    5 @ countdown  // 4 3 2 1 0

If you wish to define an applicable that accepts more than one value, you can use a more traditional syntax and specify its arguments like such:

    add = |a, b| a + b
    |3, 2| @ add == 5 

Note that in this case you don't have access to the default placeholders, and you need to stick to the calling syntax `|arguments| @ applicable`.

### OPTIONALS

TODO
