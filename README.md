# haskell-todo

This project is my first incursion into real-world-ish Haskell. The intention
here was to overengineer a toy todo-list webservice to get a glimpse at what
it would be like to use Haskell in production.

(Btw, this is by no means production-ready.)

Overall code organization is heavily based on what I've seen working with
microservices at [Nubank](https://nubank.com.br) - in a _Clojure_ codebase.
I have no idea on how it compares to typical Haskell architectures. I built
this mostly by fooling around with the language and striving for decoupling
and separation of concerns.

I've learned a lot building this simple thing. I've tried to document some of
this learning in the modules themselves, in haddock sections. I hope others can
learn a lot from this, too :)

## Building, testing, running

This project was built with [Stack](https://www.haskellstack.org/). Normal
`stack` calls should work fine:

```bash
# build haskell-todo.exe
stack build

# build and run
stack run

# run tests
stack tests

# start ghci
stack ghci
```

If you want to play with the code, I suggest running the included `ghci.sh`
script, which loads `ghci` with both tests and library modules loaded.
In that repl you can call:

```haskell
λ> main -- run all tests
...

λ> HaskellTodo.startApp -- start the app
Server running at port 8080
```

Once the server is running (from `ghci` or `stack run`), you can enter
`http://localhost:8080/index.html` to interact with the API through a very
simple web page.

Code for this front end is available [here](/static/index.html). I'm not very
proud of it, but it wasn't really the focus here ¯\\\_(ツ)_/¯
