# AyaCompiler

## Description

This compiler is based on the Oberon-0 compiler in Prof. Niklaus Wirth "Compiler Construction" book and the Oberon-07 compiler in Project Oberon (2013).

The goal is to implement the recursive descent compiler for Oberon-07 language, Intel 64 architecture.

However, since ver. 0.8, it has employed syntax tree structure, and therefore not purely single pass recursive descent anymore.

Binary is also supply with source.

## Updated 23/08/2016 - Rewrite the compiler again

Everytime I went against Prof. Wirth teaching, everytime I regretted my decision.

Modify the Oberon-07 language for the sake of adapting with the host system was a big mistake. A language is designed with a clear purpose at the beginning. However, when coping with other systems' requirements, we will make the language needlessly complicated. Complex language, complex model, etc. are great for building fancy things, but totally unsuitable for reliable operation.

More complex = more affected by noise and error in real world.

A good solution would be: let each language focus only on designed purpose and using a common interface/protocol when interaction is needed. The desire for one omnipotent language which can do everything must be discarded.

## Personal ranting

* [Niklaus Wirth was right after all](https://github.com/congdm/AyaCompiler/wiki/Niklaus-Wirth-was-right-after-all)
* [What I had learned from this compiler](https://github.com/congdm/AyaCompiler/wiki/What-I-had-learned-from-this-compiler-(part-2))

## Documentation

This compiler follows the latest [Oberon-07 Report](http://www.inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf) faithfully (hope so), with the following extensions (more exactly, complications):

### SYSTEM.LoadLibraryW and SYSTEM.GetProcAddress

**Reason for introducing:** For calling external procedures of Win32 API and other languages.

### Pragma
Here is the list of compiler pragma:

| Pragma         | Meaning                                                                                                                                 | Example usage                                                                                                                                                                                    |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `(*$MAIN*)`    | Generate .EXE file instead of .DLL                                                                                                      |                                                                                                                                                                                                  |
| `(*$CONSOLE*)` | Same as `MAIN` but for console applications                                                                                             |                                                                                                                                                                                                  |
| `(*$NEW*)`     | Install a procedure as the standard procedure `NEW`.<br> By default, `NEW` is not installed, so you need to install your own procedure. | `(*$NEW Proc*)` installs procedure `Proc` (in the same module) as `NEW` <br> `(*$NEW Mod.Proc*)` installs procedure `Proc` from module `Mod` as `NEW` (note: must import `Mod` in order to work) |

All pragma should stay at the beginning of module for easy visibility.

User-defined NEW procedure must have the formal parameters as follow:

`PROCEDURE (VAR ptr: INTEGER; tdAdr: INTEGER)`
