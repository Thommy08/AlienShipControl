# AlienShipControl
Language: Haskell  Concepts: Functional programming, pattern matching, higher-order functions, data validation 


Key Features
Command Parsing from a structured text file for multiple ships.

Supported Actions:

init: Initialize a ship's position and power state.
initspace: Define movement boundaries.
move: Move the ship in 3D space.
acao: Turn the ship on or off.


Validations for:

Movements outside defined boundaries
Negative coordinate values (especially in Z-axis)
Redundant actions (e.g., turning on a ship that's already on)
Potential ship collisions (basic validation)


Interactive Terminal Menu for:

Listing all ships
Running actions for a specific ship or all ships
Manually inserting new actions
State Updates printed to the console after each action


Future Work:

Save updated ship states to a new output file
