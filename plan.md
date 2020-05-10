
# Hexx

Small, s-expression based interpreter

overview: minimal language for self-extension

interpreted prior to llvm use

requirements:

- intrinsics
- eval
- only one control flow mechanism
- types are values?
- all functions/values/types use same type of "def"
- values are unboxed with varying sizes


Type:
 - int
 - id
 - tuple

# S-Exp improvements

Some parens are implicit, unless there are new lines awaiting a closing paren:

```
  (do
    let x 1
    let y 3
  )
```

Is the same as:

```
  (do
    (let x 1)
    (let y 3)
  )
```

But different to:

```
  (do let x 1 let y 3)
```

And different to:

```
  (do let x 1 let y 3)
```


```lisp
  (fn fib (n) ; write Fibonacci series up to n
    let a 0
    let b 1
    (loop
      (if (a < n) (
          io-write! (repr a)
          io-write! " "
          repeat b (a + b)
        )
        else (
          io-write! "\n"
          break b
        )
      )
    )
  )
```

```ruby
  def fib (n) # write Fibonacci series up to n
    let a 0
    let b 1
    loop
      if (a < n)
        io-write! (repr a)
        io-write! " "
        repeat b (a + b)
      else
        io-write! "\n"
        break b
      end
    end
  end
```
