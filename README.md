# Math Expression Parser

Small parser that I've been working on to learn Haskell

So far there are two implementations:
* *Simple*: One file that executes the operations as it parses the input
* *"Complete"*: This implementation is broken in multiple steps: Input > Token string > AST > IR > Evaluation

Both implementations can be executed by simply running the `main.hs`:

```
$ cd simple # or complete
$ runghc main.hs '<expression>'
```
