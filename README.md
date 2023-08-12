# Dynamik
A fast, powerful, expressive and typesafe language.

Note: The language is still WIP but its pretty functional now.

## Installation:
To install `dyc` (**dy**namik **c**ompiler) make sure `cargo` is installed and run the following command:

```sh
$ cargo install --git https://github.com/dynamik-lang/Dynamik
```

And there you go!

## Quick start
### Printing your first hello world in Dynamik:
There's no standard library right now, so we have to use printf from libc.

File: hello_world.dy
```dynamik
extern "C" let printf(string, ...) -> int;

printf("Hello, World!\n");
```

### Running your first code:
#### Compiling the code:

```sh
$ dyc compile hello_world.dy    # to compile
$ ./hello_world                 # to run the executable
Hello, World!
```

#### Jit executing the code:

```sh
$ dyc run hello_world.dy
Hello, World!
```

## Language Reference
This is what the language supports so far.
Note: Since the language is work in progress everything in this section is the subject to change.

## Comments
In Dynamik, like any other language, comments are used to add notes or describe functionality in the code. They are not executed as part of the program, but provide valuable context and explanation to developers reading the code.

Dynamik uses single-line comments which start with --. Here's an example:

```dynamik
-- This is a comment in Dynamik
let a: int = 1; -- You can also write comments after code
```

## Data Types
Dynamik supports the following data types:

- `string`: A sequence of characters.
- `int`: A 64-bit signed integer.
- `float`: A 64-bit floating-point number.

## Let
To define a variable use the `let name: type = value;` syntax. For example:

```dynamik
let a: int = 1;
let b: int = a + 10;
```

## Basic Math Operators
Dynamik supports the following basic math operators:

<table>
  <tr>
    <th>Operator</th>
    <th>Description</th>
    <th>Example</th>
  </tr>
  <tr>
    <td>+</td>
    <td>Addition: This operator adds two numbers together.</td>
    <td><code>let a: int = 1 + 2;</code> sets <code>a</code> to <code>3</code>.</td>
  </tr>
  <tr>
    <td>-</td>
    <td>Subtraction: This operator subtracts the second number from the first.</td>
    <td><code>let a: int = 5 - 2;</code> sets <code>a</code> to <code>3</code>.</td>
  </tr>
  <tr>
    <td>*</td>
    <td>Multiplication: This operator multiplies two numbers together.</td>
    <td><code>let a: int = 2 * 3;</code> sets <code>a</code> to <code>6</code>.</td>
  </tr>
  <tr>
    <td>/</td>
    <td>Division: This operator divides the first number by the second.</td>
    <td><code>let a: int = 6 / 2;</code> sets <code>a</code> to <code>3</code>.</td>
  </tr>
  <tr>
    <td>%</td>
    <td>Modulus: This operator returns the remainder of the first number divided by the second.</td>
    <td><code>let a: int = 5 % 2;</code> sets <code>a</code> to <code>1</code>.</td>
  </tr>
  <tr>
    <td>!</td>
    <td>Logical NOT: This operator inverts the value of a boolean.</td>
    <td><code>let a: bool = !true;</code> sets <code>a</code> to <code>false</code>.</td>
  </tr>
  <tr>
    <td>-</td>
    <td>Negation: This operator changes the sign of a number.</td>
    <td><code>let a: int = -3;</code> sets <code>a</code> to <code>-3</code>.</td>
  </tr>
</table>

## Functions
You can define functions in Dynamik using the following syntax:

```dynamik
let print_hello() {
    printf("Hello World");
}
```

Functions can also accept arguments:

```dynamik
let add(v1: int, v2: int) -> int {
    return v1 + v2;
}
```

In the above example, the `add` function takes two `int` arguments and returns an `int` value.

## External Functions
Dynamik allows you to use external functions from the C library. To use an external function, you can declare it as follows:

```dynamik
extern "C" let puts(string) -> int;
extern "C" let printf(string, ...) -> int;
```

The `...` at the end indicates that the function can accept a variable number of arguments, making it variadic.

## If-Else Statements
Dynamik supports if-else statements for controlling program flow. For example:
Note: Dynamik doesn't supports

```dynamik
let a: int = 3;
let b: int = a + 1;

if a != b {
    printf("A is not equal to B");
} else {
    printf("A is equal to B");
}
```

In the above code, the if-else statement checks whether `a` is equal to `b` or not, in each case, it prints a message.

## While Statements
Dynamik also supports while loops. For example:

```dynamikdy
let x: int = 0;
while x < 5 {
    printf("%d\n", x);
    x = x + 1;
}
```

In the above code, the while loop prints the value of `x` while it is less than 5.

## Modules
Dynamik allows you to create modules to organize your code and create namespaces. For example:

```dynamik
mod math {
    let add(v1: int, v2: int) -> int {
        return v1 + v2;
    }
}
```

In this example, we create a module named `math`. The module contains a function `add` adds two values and returns the result. Using the `math` module, you can call the `add` function as follows:

```dynamik
printf("1 + 2 = %d\n", math::add(1, 2));
```

### TODO

[ ] Add Array slices with fixed size
[ ] Add structs & more types
[ ] Add traits and impl blocks
[ ] Add pointers
[ ] Add dependent typing
[ ] Create std library
[ ] Work on the package manager
