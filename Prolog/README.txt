Progetto Linguaggi di Programmazione: JSON Parser
15 Gennaio 2023

Gruppo:
875068 Viviana Giuliani
869310 Daniel Marco Gatti

The project analyzes a string and trasforms it into a JSON 
providing a compatible module in Prolog first.
It can retrieve a certain value from a JSON given a list of parameters.
Writes and reads a JSON to/from a file.

1) jsonread(Filename, JSON), to get the Prolog compatible form of the .json file
2) jsondump(JSON, Filename), the json will be written to a filename file.

You can also use the program with a given string by calling jsonparse(Jstring, Object).