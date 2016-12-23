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
