Readme file for parser.exe

This program is designed to read in each line from a user-specified file (such as InitInt.txt) in the format of: KEYWORD = VALUE. These are then parsed into internal variables.
KEYWORDS are defined in keylist.txt for ease of reference and extension. To extend the key list:
1) add a new key to keylist.txt in lowercase only and define its purpose.
2) add the appropriate variable to MODULE INITDATA in parser.f95
3) add a new ELSEIF statement to the IF tree in READINIT to match the key = value in parser.f95.
4) recompile parser.exe from parser.f95

The program can handle values of type integer, string and boolean.
The maximum size of KEYWORDs and VALUEs are 75 characters.
The maximum total length of each line is 150 characters.
The maximum length of the filename and (relative) path is 20 characters.
These limits are arbitrary to limit memory usage; they can be increased through modification of MODULE ARRAYLEN.

The parser has significant error handling and can detect incorrectly defined VALUEs, lack of "=" deliminators and incorrect filenames. The intention in design of this program was to allow incorporation into a larger program as an init file reading subroutine.

The .dll files included are required to run the version of parser.f95 compiled in cygwin.