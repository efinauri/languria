__ABOUT__

languria is a toy programming language made for self-teaching and personal use, written by referencing the excellent [crafting intepreters](https://craftinginterpreters.com/) book by R. Nystrom. 
The name ("the watermelon" in italian) is the first thing that came to mind that starts with lang-. Appearently it's also a [beetle](https://en.wikipedia.org/wiki/Languria), which might as well make for a nice mascot!

__SYNTAX (in progress)__ 

- __PRINT__

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
