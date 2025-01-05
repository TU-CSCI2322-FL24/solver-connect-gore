# solver-connect-gore
Project 5: Connect Four Solver
* Team Name: Connect Gore
* Team Members: Rudra Vashi, Aidan McLoughlin, Kevin Han, Owen Flynn, Erin Heath

Run Solver.hs or the executable solve to play. Add a game file as a command line argument or when prompted, and if no flags are provided the solver will output a good move given the current game state.

The following flags are supported:
-h        --help           Prints help message and quits
-w        --winner         Prints the definitive best move and outcome
-d <num>  --depth <num>    Prints a good move by analyzing all games after <num> turns
-m <move> --move <move>    Prints the board after making a move
-v        --verbose        Prints a good move and a description of how good the move is
-i        --interactive    Starts a new game against the computer or plays against the computer from the game state of a file-name argument
