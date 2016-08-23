MODULE SymTable0;

IMPORT
	Base := Base0, Scanner := Scanner0;
	
VAR
	curLev*: INTEGER;
	universe*, topScope*: Base.Object;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*(name: Base.IdStr);
	VAR scope: Base.Object;
BEGIN
	NEW(scope); scope.class := Base.cHead; scope.name := name;
	scope.next := NIL; scope.dsc := topScope; topScope := scope
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

PROCEDURE Find*(VAR obj: Base.Object; id: Base.IdStr);
	VAR p: Base.Object;
BEGIN
	p := topScope.next; WHILE (p # NIL) & (p.name # id) DO p := p.next END;
	IF (p = NIL) & (topScope # universe) THEN
		IF id # topScope.name THEN p := universe.next
		ELSE p := topScope.dsc.next
		END;
		WHILE (p # NIL) & (p.name # id) DO p := p.next END
	END;
	obj := p
END Find;

PROCEDURE New*(VAR obj: Base.Object; id: Base.IdStr; cls: INTEGER);
END New;
	
PROCEDURE Init*(modid: Base.IdStr);
BEGIN
	topScope := NIL; OpenScope(''); universe := topScope
END Init;
	
BEGIN
END SymTable0.