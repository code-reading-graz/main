

# Functions First

A code reading discussion on functional programming architecture and design
patterns.

Functional Programming (FP) is a programming paradigm—a way of thinking about
and structuring software—that treats computation as the evaluation of
mathematical functions. Unlike other paradigms that emphasize sequences of
commands and changing state, functional programming focuses on applying
functions, often avoiding shared state, mutable data, and side effects.

History lambda calculus in the 1930s.

Great for: Multi-Core Processing, Parallelism, Concurrency, Big Data,
Robustness.

Focus on the *what* and *how* of the code, not the *where* and *when*.

Program is build on on function that is build based on many very small
functions.

Multi-paradigm languages inherit the functional programming paradigm, like
JavaScript, Python, Java, C#, Go, Ruby, etc.

Immutability in practice: Create new data.

```elixir
x = 1 # bind a value to a variable
if true do
  x = 2
end
IO.puts x # prints 1
```

https://hexdocs.pm/elixir/List.html

No loops, instead rely on recursion.

## Glossary

- **pure function** ... given the same input, will always return the same
  output and will not cause any observable changes outside their scope. In
  other words, deterministic output and no side effects.
- **side effect** ... a change in state or observable interaction with the
  outside world that occurs during the execution of a function. Examples
  include modifying a global variable, writing to a file, or printing to the
  console, remote interaction (e.g., network calls).
- **higher-order function** ... a function that can take other functions as
  arguments or return a function as its result. This allows for more
  abstract and flexible programming styles, enabling the creation of
  functions that can operate on other functions, such as `map`, `filter`, and
  `reduce`.
- **first-class function** ... a programming language feature that treats
  functions as first-class citizens, meaning they can be passed as arguments to
  other functions, returned from other functions, and assigned to variables.
- **closure** ... a function that captures the lexical scope in which it was
  declared, allowing it to access variables from that scope even when the
  function is executed outside of that scope. This enables data hiding and
  encapsulation.
- **recursion** ... a programming technique where a function calls itself in
  order to solve a problem. It can be used to break down complex problems into
  simpler subproblems, but care must be taken to ensure that the recursion
  terminates properly.
- **tail recursion** ... a specific form of recursion where the recursive call
  is the last operation in the function. This allows for optimizations by the
  compiler or interpreter, reducing the risk of stack overflow and improving
  performance. Often, tail call optimization (TCO) is used to convert the
  recursive call into a loop, allowing for constant stack space usage.
- **memoization** ... a technique used to optimize the performance of recursive
  functions by caching the results of expensive function calls and reusing
  them when the same inputs occur again.
- **currying** ... a technique of transforming a function that takes multiple
  arguments into a sequence of functions, each taking a single argument. This
  allows for partial application and can lead to more reusable and modular
  code.
- **partial application** ... a technique where a function is applied to some
  of its arguments, producing a new function that takes the remaining
  arguments. This allows for the creation of specialized functions from more
  general ones.
- **function composition** ... the process of combining two or more functions
  to produce a new function. This allows for the creation of complex behavior
  by chaining simple functions together.
- **monad** ... a design pattern used to handle side effects in functional
  programming. It provides a way to structure computations and manage
  side effects in a consistent manner, allowing for better code organization
  and readability.

Available in dedicated languages (Haskell, Clojure, Erlang, Elixir, Scala, F#)
or as a paradigm in other languages (JavaScript, Python, Java, C#, Go, Ruby,
etc.). Also Elm, OCaml.

## Downsides, Potential Tradeoffs

- **Performance** ... functional programming can introduce overhead due to
  immutability and recursion, which may lead to performance issues in some
  cases. However, many modern functional languages have optimizations to
  mitigate these concerns.
- **Learning Curve** ... functional programming can be challenging for developers
  who are used to imperative or object-oriented paradigms. The concepts of
  immutability, higher-order functions, and recursion may require a shift in
  thinking.
- **Debugging** ... debugging functional code can be more difficult due to
  the lack of side effects and mutable state. This can make it harder to
  trace the flow of data and understand how functions interact with each
  other.

## Effective Applications

Web, UI, Telecom & Mobile, Distributed Systems, Finance (Haskell), Data Science

- CoucheDB (Erlang, Elixir)
- Pandoc (Haskell)
- LiveBook (Elixir)
- Plausible Analytics (Elixir)

## Code

```haskell
-- Factorial function in Haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n > 0 = n * factorial (n - 1)
            | otherwise = error "Factorial is not defined for negative numbers"

-- Example usage:
main :: IO ()
main = print (factorial 5) -- Output: 120
```

```fsharp
// Factorial function in F#
let rec factorial n =
    match n with
    | 0 -> 1
    | x when x < 0 -> failwith "Factorial is not defined for negative numbers"
    | x -> x * factorial (x - 1)

// Example usage:
printfn "%d" (factorial 5) // Output: 120
```

```ocaml
(* Factorial function in OCaml *)
let rec factorial n =
  match n with
  | 0 -> 1
  | x when x < 0 -> invalid_arg "Factorial is not defined for negative numbers"
  | x -> x * factorial (x - 1)

(* Example usage: *)
let () = Printf.printf "%d\n" (factorial 5) (* Output: 120 *)
```

```scale
// Factorial function in Scala (with tail recursion)
import scala.annotation.tailrec

object MathUtils {
  def factorial(n: BigInt): BigInt = {
    if (n < 0) throw new IllegalArgumentException("Factorial not defined for negative numbers")

    @tailrec
    def factHelper(x: BigInt, accumulator: BigInt): BigInt = {
      if (x <= 1) accumulator
      else factHelper(x - 1, x * accumulator)
    }
    factHelper(n, 1)
  }
}

// Example usage:
object Main extends App {
  println(MathUtils.factorial(5)) // Output: 120
}
```

```elixir
# Factorial function in Elixir
defmodule MathUtils do
  def factorial(0), do: 1
  def factorial(n) when n < 0, do: raise "Factorial not defined for negative numbers"
  def factorial(n), do: n * factorial(n - 1)
end
# Example usage:
IO.puts MathUtils.factorial(5) # Output: 120
```

```javascript
// Factorial function in JavaScript (using recursion)
function factorial(n) {
  if (n < 0) throw new Error("Factorial not defined for negative numbers");
  return n === 0 ? 1 : n * factorial(n - 1);
}
// Example usage:
console.log(factorial(5)); // Output: 120
```
