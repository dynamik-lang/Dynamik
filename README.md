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
extern "C" let printf(string, ...);

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
extern "C" let puts(string);
extern "C" let printf(string, ...);
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
