
Using Haskell library `sbv` to solve the second puzzle given here:

https://www.cse.chalmers.se/~sattler/retreat-puzzle-2024.pdf

`sbv` is a front-end to SMT provers like `z3`.

Files:

- [Puzzle definition](src/Val.hs)
- [Board](src/Board.hs) and constraints concerning correct division and coloring of the board.
- [Correct sums](src/Valuation.hs)
- [Entry point](main/Main.hs)

See comments for the representation of the board and the constraints that classify valid solutions.
