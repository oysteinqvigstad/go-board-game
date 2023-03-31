# PROG2006 Assignment 1

## Build and run instructions

### Prerequisites
- GHC including stack
- SDL2
- SDL2-dev
- SDL2-ttf
- SDL2-image


### Compile and run the program

Running this command from the project root will first build the application (if it has not been built already or if there are changes in the source code) and then execute it:

    stack run

Additionally, the program will attempt to load the game.sgf file from the project directory, if present.

## Requirement Specifications

### Requirements specified by the lecturer

**Mandatory Requirements**: These are the requirements that must be met for a passable grade

- Visually represent game state and the stones on the board
- Seamlessly update board state with each game move.
- Accurately detect liberties and captures
- Effectively load board state from file according to SGF specs for ease of use. 
- Automatically prevent illegal game moves, with the exception of the ko rule.
- Correctly capture stones according to the rules of the game.
- Operate continuously without interruption for more than 30 seconds.

**Optional Requirements**: These are the requirements that would count as extra credit

- The program should include a graphical user interface with intuitive mouse interaction for placing stones.
- Unless White played last in the SGF file, the black stone should make the first move. 
- The graphical user interface should be resizable, allowing users to adjust the size of the application window.
- The program should enforce the "ko rule" to prevent illegal game moves.
- The program should include a check to determine if the boards are isomorphic for translation, rotation, and symmetry.
- The program should include an "undo" function, allowing users to revert to a previous game state.
- The board state should be updatable via computer-generated moves, which could include:
  1. Identifying a single move that eliminates the most stones.
  2. Identifying a single move that extends the degrees of freedom for a group.
  3. Selecting a random legal move from the available options.

### Requirements assumed as developer
- The program will implement the New Zealand derived and Ing ruleset for placing stones, allowing self-capturing moves, commonly known as "suicide" moves, to be legal.
- The graphical user interface (GUI) will scale its contents proportionally as the window is resized. During scaling, all cursor interactions will be disabled to prevent unintended interactions.
- The filename for the SGF file will be automatically generated and hardcoded into the program, as user selection of the filename is not a requirement.
- The Drag-and-Drop feature is optional, and instead, the program will allow users to interact with the board by clicking on the relevant positions.
- The coordinate system used in the program will be based on the convention where the origin, (0,0), is located in the upper left corner of the board.
- The program will not implement end-game logic, but it will accurately track the number of prisoners each player has taken. The GUI should include a representation of the number of prisoners each player has captured.

## Non-functional Requirements

1. Performance:
    - Response time: The program should draw the stone selector without noticeable delay when hovering over intersections, and placing a stone should feel immediate.
    - Throughput: The program should not experience excessive overhead when parsing SGF files during startup.

2. Scalability:
    - The program should be designed to accommodate future growth in functionality with ease, maintaining scalability for any potential future updates.

3. Usability:
    - User Interface: The program should have a simple yet intuitive, easy-to-use interface for placing stones.

4. Reliability:
    - Stability: The program should handle errors gracefully and never crash, being resilient to user input.

5. Maintainability:
    - Code Quality: The code should adhere to established coding standards and be written following best practices.
    - Documentation: All functions must be documented, with most non-input/output functions having a doctest. The program should include a document detailing instructions, functional dependencies, non-functional dependencies, and a self-assessment specification.
6. Compatibility:
    - Operating system support: The program should be independent of operating system-specific libraries, allowing it to run on any operating system with support for SDL2 and the Haskell compiler.
 
## Assessment Specifications
The application should:

- Meet the functional requirements listed under "must have" outlined in the "Requirement Specification".
- Meet the non-functional requirements specified in the "Non-functional Requirements" section.
- Include detailed instructions on how to run the program in the documentation.
- Have a merge request to the PROG2006-labs repository for version control of source files.
- Use the issue tracker proactively and tie commits to relevant issues.
- Maintain professionalism while writing all documents.
- Eliminate ambiguity by declaring assumptions based on the functional requirements.
- Pass all unit tests.
- Include comments in functions that explain their purpose, particularly if it is not immediately apparent.