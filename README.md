# Go Game

## About

A graphical implementation of the ancient board game Go, featuring basic [SGF](https://homepages.cwi.nl/~aeb/go/misc/sgf.html) (Smart Game Format) file parsing capabilities. The game is built using Haskell and SDL2 bindings, combining functional programming with graphics rendering.

### Why Haskell?
While Haskell's purely functional paradigm might seem unconventional for a game centered around state changes and side-effects, this project was part of the Programming Language course in my second year at NTNU. The implementation provided valuable insights into:

- **Monadic Abstractions**: Managing game state and IO operations through functional patterns and monads
- **Algebraic Data Types**: Modeling game elements and rules using sum and product types, leveraging the type system for correctness
- **Pattern Matching**: Implementing game logic through Haskell's expressive pattern matching
- **Pure Game Logic**: Separating pure game rules from side effects, making the code more testable and maintainable

The project served as a practical exercise in applying functional programming concepts, and how Haskell's strict separation of pure and impure code can structure even UI applications. While more conventional choices like C++, Python, or Java might be more practical for game development, using Haskell provided valuable insights into functional programming.

A show-and-tell video created for class:

https://github.com/user-attachments/assets/502c349d-1bc9-4fa7-b0e6-2bc99af2bfe2

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

### Instructions

To initiate the game with specific player names and a custom board size, it is necessary to provide an SGF-formatted file named "game.sgf" in the project root directory. Please note that this is the sole method of loading player names and modifying board size within the application.

Gameplay control is limited to mouse input, specifically by selecting an intersection when the highlighter is displayed. The highlighter will alternate between black and white with each move, indicating the active player's turn. The application window may be resized to suit different screen resolutions.

At present, the game does not support the ability to save the state to file.

### Project structure


    ├── app             
    │ └── Main.hs           // Entry point of the applcation
    ├── assets              // Textures
    ├── game.sgf            // SGF formated game state
    ├── src                 
    │ ├── Board.hs          // Board logic
    │ ├── DataTypes.hs      // Global data types 
    │ ├── Game.hs           // Game initialization and core game loop
    │ ├── Graphics.hs       // SDL related functions
    │ └── Text.hs           // SGF parser
    └── ttf                 // Fonts



## Requirement Specifications

### Requirements specified by the lecturer

**Mandatory Requirements**: These are the requirements that must be met for a passable grade

- Visually represent game state and the stones on the board
- Seamlessly update board state with each game move.
- Accurately detect liberties and captures
- Effectively load board state from file according to SGF specs for ease of use. 
- Automatically prevent illegal game moves, with the exception of the ko rule.
- Correctly capture stones according to the rules of the game.
- Unless black stone played last in the SGF file, the black stone should make the first move.
- Operate continuously without interruption for more than 2 seconds.

**Optional Requirements**: These are the requirements that would count as extra credit

- The program should enforce the "ko rule" to prevent illegal game moves.
- The program should use SDL2 for graphical user interface with intuitive mouse interaction for placing stones.
- The graphical user interface should be resizable, allowing users to adjust the size of the application window.
- Stones and the game board should be represented with textures
- The program should include an "undo" function, allowing users to revert to a previous game state.
- The program should include a check to determine if the boards are isomorphic for translation, rotation, and symmetry.
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
    - Testing: Unit tests should cover roughly 60 percent of the program code 

5. Maintainability:
    - Code Quality: The code should adhere to established coding standards and be written following best practices.
    - Programming language: The game must be written in either Haskell or Rust
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

## Self-Assessment Report

### Functional Requirements

All the mandatory requirements have been implemented, including some optional. For reference, here is a simple checklist that summarizes the requirements outlined in the requirement specification.

**Mandatory**:
- [x] Visually represent game state and the stones on the board
- [x] Seamlessly update board state with each game move.
- [x] Accurately detect liberties and captures
- [x] Effectively load board state from file according to SGF specs for ease of use.
- [x] Automatically prevent illegal game moves, except the ko rule.
- [x] Correctly capture stones according to the rules of the game.
- [x] Unless black stone played last in the SGF file, the black stone should make the first move.
- [x] Operate continuously without interruption for more than 2 seconds.

**Optional**:
- [x] The program should enforce the "ko rule" to prevent illegal game moves.
- [x] The program should use SDL2 for graphical user interface with intuitive mouse interaction for placing stones.
- [x] The graphical user interface should be resizable, allowing users to adjust the size of the application window.
- [x] Stones and the game board should be represented with textures
- [ ] The program should include an "undo" function, allowing users to revert to a previous game state.
- [ ] The program should include a check to determine if the boards are isomorphic for translation, rotation, and symmetry.
- [ ] The board state should be updatable via computer-generated moves, which could include:
    - [ ] Identifying a single move that eliminates the most stones.
    - [ ] Identifying a single move that extends the degrees of freedom for a group.
    - [ ] Selecting a random legal move from the available options.

### Non-functional Requirements

1. **Performance**: The game launches almost instantly, even when reading from SGF, and is highly responsive to user input. Testing have not identified any significant delays or issues with the frame rate.

2. **Scalability**: The application has been designed with a certain degree of modularity in mind, comprising five distinct modules. These modules include a parser, graphics, game logic, data types, and a final module responsible for the game loop and setup.

3. **Usability**: As the program only accepts input from the mouse, it should be fairly intuitive for users to learn how to place stones. The process involves clicking on empty intersections, with no additional actions required.

4. **Reliability**: The heuristic testing did not reveal any performance issues or crashes. The code contains approximately 76 unit tests, all of which have passed validation as anticipated. Unfortunately, the HPC tool was unable to generate a test coverage report for reasons unknown, so the precise level of test coverage is currently difficult to determine.
It is important to note that the SGF parser has not undergone comprehensive testing for edge cases. Therefore, it is nondeterministic what will occur should variants be added to the game file, as this feature is not currently supported.

5. **Maintainability**: Each function within the codebase have been documented to clearify its intended purpose, and most non-IO functions have undergone unit testing. The game itself has been implemented in Haskell and SDL, and adheres to a consistent code style.
All relevant documentation, including functional and non-functional requirements, as well as an assessment spcification, has been included with a professional tone in mind. It is worth noting that the game has been exclusively been tested within a Linux environment, although it should be compatible with other platforms provided that the necessary SDL development libraries are accessible.


### Reflection

This task proved to be challenging and time-consuming for me. Due to my lack of familiarity with Haskell and SDL, I had to refactor the code multiple times to accommodate its growing complexity.

The need for refactoring arose primarily due to the exponential increase in the number of parameters that needed to be passed around. To address this issue, I resolved to encapsulate all relevant data within a single data structure, which necessitated modifying the entire codebase. This is typical when working on a project with limited prior experience.

Despite these challenges, I am pleased to report that I was able to fulfill all mandatory requirements and even completed some optional ones. The game appears to be performing well, and I am satisfied with the outcome of my efforts.

In retrospect, I recognize that I could have managed the project more effectively. Specifically, I began working on the assignment early on and was not aware of the requirement to fork the lab-repository in order to make a merge request for the submission. Consequently, I had to migrate the existing project to a new workspace, resulting in the loss of previous commit history and issues, which may be perceived as unprofessional.

