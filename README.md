# Patchouli Oberon-07 Compiler

## Description

This compiler is based on Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement a recursive descent compiler for Oberon-07 language, Intel 64 architecture.

Previously named AyaCompiler, which was a single pass compiler. However, since the current version (which employs a syntax tree) is considerably different from the old one, I decide to change its name.

Binary is also supply with source.

## Documentation

This compiler follows the latest [Oberon-07 Report](http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf) faithfully (hope so), with the following extensions (more exactly, complications):

### SYSTEM.LoadLibraryW and SYSTEM.GetProcAddress

**Reason for introducing:** For calling external procedures of Win32 API and other languages.

**Note: Their formal parameters are as follow:

`PROCEDURE LoadLibraryW(VAR result: INTEGER; lpFileName: ARRAY OF CHAR);`

`PROCEDURE GetProcAddress(VAR result: INTEGER or any procedure variable; hModule, lpProcName: INTEGER);`

**Example usage:

```
PROCEDURE ImportProc;
VAR user32, i: INTEGER;
	ansiStr: ARRAY 256 OF BYTE;
	str: ARRAY 256 OF CHAR;
	MessageBoxW: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER);
BEGIN
	SYSTEM.LoadLibraryW(user32, "USER32.DLL");
	ASSERT(user32 # 0); str := "MessageBoxW"; i := 0;
	WHILE str[i] # 0X DO ansiStr[i] := ORD(str[i]); INC(i) END; ansiStr[i] := 0;
	SYSTEM.GetProcAddress(MessageBoxW, user32, SYSTEM.ADR(ansiStr));
	MessageBoxW(0, SYSTEM.ADR("Text"), SYSTEM.ADR("Caption"), 0)
END ImportProc;
```

### Pragma
Here is the list of compiler pragma:

| Pragma         | Meaning                                                                                                                                 | Example usage                                                                                                                                                                                    |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `(*$MAIN*)`    | Generate .EXE file instead of .DLL                                                                                                      |                                                                                                                                                                                                  |
| `(*$CONSOLE*)` | Same as `MAIN` but for console applications                                                                                             |                                                                                                                                                                                                  |
| `(*$NEW*)`     | Install a procedure as the standard procedure `NEW`.<br> By default, `NEW` is not installed, so you need to install your own procedure. | `(*$NEW Proc*)` installs procedure `Proc` (in the same module) as `NEW` <br> `(*$NEW Mod.Proc*)` installs procedure `Proc` from module `Mod` as `NEW` (note: must import `Mod` in order to work) |

All pragma should stay at the beginning of module for easy visibility.

User-defined NEW procedure must have the formal parameters as follow:

`PROCEDURE (VAR ptr: INTEGER; tdAdr: INTEGER);`

See PROCEDURE New in module BaseSys for example implementation. Type desc structure is same as in Project Oberon, with each word be 64-bit, instead of 32-bit, and the max level of extension is 7, instead of 3.
