MODULE SymTable0;

IMPORT
	Base := Base0, Scanner := Scanner0;
	
CONST
	normalModule = 0;
	lowLevelModule = 1;
	
VAR
	curLev*: INTEGER;
	universe*, topScope*: Base.Object;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*(name: Base.IdStr);
	VAR scope: Base.Object;
BEGIN
	NEW(scope); scope.class := Base.cHead; scope.name := name;
	scope.next := Base.guard; scope.dsc := topScope; topScope := scope
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;
	
PROCEDURE Init*(modid: Base.IdStr);
BEGIN
	topScope := NIL; OpenScope(''); universe := topScope
END Init;

PROCEDURE New*(VAR obj: Base.Object; id: Base.IdStr; cls: INTEGER);
END New;
	
BEGIN
END SymTable0.