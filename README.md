# PROG2006 Assignment 1

## Build and run instructions

### Prerequisites
- GHC including stack
- SDL2
- SDL2-dev
- SDL2-fft
- SDL2-image


### Compile and run the program

This command will first build the application (if it has not been built already or if there are changes in the source code) and then execute it:

    stack run


Optionally, the application can be launched with a SGF file, which will load in the stored game state:

    // WARNING: NOT IMPLEMENTED YET
    stack run -- filename
    example:
    stack run -- ./gamedata.sgf


## Requirement Specifications

### Requirements specified by the lecturer

**Must have**: These are the requirements that must be met for a passable grade

- Represent game state
- Represent stones on the board
- Detect liberties and groups
- Loading board state from SGF files
- Updating board state (game moves)
- Prevent illegal game moves (ko rule excluded)
- Ability to properly capture stones
- The program should never halt for more than 30 seconds

**Nice to have**: These are the requirements that would count as extra credit

- GUI with mouse interaction
- Black stone should start first (unless White played last in SGF file)
- Resizing window
- Enforce "ko rule"
- Checking if boards are isomophic for translation, rotation and symmetry
- Undo function
- Updating the board state via computer-generated moves, for example:
    1. Find a single move that eliminates the most stones
    2. Find a single move that extends the degrees of freedom for a group
    3. Random legal move

### Requirements assumed as developer
- Self-capturing moves (known as "sucide") are legal (New Zealand derived and Ing ruleset)
- The contents of the GUI should scale as the window is resized. When scaling is performed, any cursor interaction should be invalidated.
- A fixed file path for where the SGF file is unspecified. We can import it with program argument
- Drag-and-Drop feature is just a suggestion. I prefer clicking on the board instead of dragging
- Coordinate (0,0) is in the upper left corner.
- Implementing end-game logic is not a requirement. But keeping track of how many prisoners
  (i.e. captured) each player has taken is. There should be some representation of how many prisoners each player has 


## Non-functional Requirements

1. Performance:
   - Response time: There should be no noticeable delay for drawing the stone selector when hovering over intersections. Placing a stone should feel immediate. 
   - Throughput: Parsing SGF files should not add excessive overhead to the program startup time.

2. Scalability:
   - The application should be designed to easily accommodate future growth in terms of functionality.

3. Usability:
    - User Interface: The application should have a very simple yet intuitive, and easy-to-use interface for placing stones.

4. Reliability:
    - Stability: The program should handle errors gracefully and should not under any circumstance crash. The program must be resilient to user input

5. Maintainability:
    - Code Quality: The code should be written following best practices and adhere to established coding standards.
    - Documentation: The functions must be documented and most non IO-functions should have a doctest. The program will include a document with instructions, functional dependencies, non-functional dependencies and a self-assessment specification.

6. Compatibility:
    - Operating system support: The application should not be relient on operating system specific libraries and should therefor run on any operating system with support for SDL2 and haskell compiler

## Assessment Specifications

- The application should meet the functional requirements listed under "must have" outlined in "requirements specified by the lecturer" in the section "Requirement Specification".
- Additionally, the application should meet the non-functional requirements, specified in the section "Non-functional Requirements".
- The documentation should include instructions on how to run the program
- The source files should have a merge request to the PROG2006-labs repository.
- Issue tracker should be used proactively and commits should be tied to said issues 
- All documents should be written with professionality in mind
- Ambiguity should be eliminated by declaring assumptions based on the functional requirements
- All units test should pass
- Functions should have comments that explain what the function does, if it is not obvious