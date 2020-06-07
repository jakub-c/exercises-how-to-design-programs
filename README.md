# Exercises from the book How To Design programs

Documentation of my journey through [How to Design Programs, Second Edition](https://htdp.org). A great book about structured approach to program design.

![How to Design Programs book cover](https://htdp.org/htdp-2e-cover.gif)

## Notes

### Design recipe

The book emphasizes a structured approach towards writing programs. It's a great alternative to the usual ad-hoc style seen in many places, this quote illustrates this really well:

> All this sounds rather complex, and you might wonder why we don’t just muddle our way through, experimenting here and there, leaving well enough alone when the results look decent. This approach to programming, often dubbed “garage programming,” is common and succeeds on many occasions; sometimes it is the launching pad for a start-up company. Nevertheless, the start-up cannot sell the results of the “garage effort” because only the original programmers and their friends can use them. [source](https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3ahtdp%29)

#### The recipe

1. Create a signature, a purpose statement and a stub of a function
2. Write a function's template
3. Write examples as test cases
4. Code the body of the function
5. Debug and refine

It's a structured version of a TDD.

---

### TODO

- [ ] add notes about natural, structural and generative recursion
- [ ] add notes about accumulators
- [ ] complete the missing exercises from [Intermezzo 4: The Nature of Numbers](https://htdp.org/2018-01-06/Book/i4-5.html)
- [ ] edit, commit and push exercises from chapters 02 and 03
- [ ] finish 449, write a helper function
- [ ] refactor 457 to something more readable and elegant
- [ ] finish 460, 461
- [ ] refactor 476, make it more functional and readable
- [ ] finish 477
- [ ] add a note based on [Concrete Time, Abstract Time](https://htdp.org/2018-01-06/Book/i5-6.html#%28part._.Concrete_.Time__.Abstract_.Time%29)
  - describe "order of n steps” phrase
  - describe "abstract running time"
  - note that we measure the performance of a program by looking at the number of recursive calls its making
  - add missing exercises from Intermezzo 5: The Cost of Computation
- [ ] finish 492
- [ ] finish 498
- [X] ~~refactor 469 to fold~~

Special thanks to [bgusach/exercises-htdp2e](https://github.com/bgusach/exercises-htdp2e) for publishing the journey through the book. It helped when I got stuck on some exercises.
