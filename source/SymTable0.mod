MODULE SymTable0;

IMPORT
	Base := Base0, Scanner;
	
CONST
	normalModule = 0;
	lowLevelModule = 1;
	
VAR
	curLev*: INTEGER;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Init*(modid: Base.IdentStr);
BEGIN
END Init;
	
BEGIN
END SymTable0.