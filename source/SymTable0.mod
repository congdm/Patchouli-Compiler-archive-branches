MODULE SymTable0;

IMPORT
	Base := Base0, Scanner;
	
CONST
	normalModule = 0;
	lowLevelModule = 1;
	
VAR
	curLev*: INTEGER;
	universe*, topScope*: Base.Object;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*(name: Base.IdentStr);
	VAR scope: Base.Object;
BEGIN
	NEW(scope); scope.class := Base.cHead; scope.name := name;
	scope.next := Base.guard; scope.dsc := topScope; topScope := scope
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;
	
PROCEDURE Init*(modid: Base.IdentStr);
BEGIN
	topScope := NIL; OpenScope(''); universe := topScope
END Init;
	
BEGIN
END SymTable0.