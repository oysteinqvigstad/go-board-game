# prog2006-assignment-1

## Assignment 1


* **NOTE** For generic professionalism requirements refer to [the Course Wiki page](https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2023/-/wikis/home#assignments)

**NOTE - THIS IS NOT a requirement specification!**

* Simple module for visualising and analysing Go game problems.
   * Represent the game state
   * Represent the stones on the board
   * Represent the groups 
* Program can answer simple game state questions like: 
   * How many white (black) groups are on the board?
   * How many degrees of freedom a given group has? 
* Loading board state from SGF files.
* Updating the board state (game moves). 
* Ability to detect illegal game moves.
* Ability to properly capture stones.
* Updating the board state via computer-generated moves, for example:
   * Find (a set) a single white move that eliminates the most black stones (exhaustive search)
   * Find (a set) a single white move that extends the degrees of freedom of a group
* For `<C` simple keyboard entry and terminal printouts are sufficient as UI.
* **Bonus for `>=C`** GUI (or TUI) and mouse interactions included. 
* **Bonus for `>C`** Write a functions that returns `True` if two board states are isomorphic. Use translation, rotation, and symmetry operations for isomorphism tests. We assume a stones and groups must maintain the exact freedom degree counts. Or, in other words, given two SGF files with a board state only, the program checks if the two games are the same (isomorphic for translation, rotation and symmetry).
