MODULE Generator0;

IMPORT
	Base := Base0,
	Scanner := Scanner0,
	SymTable := SymTable0;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FitInReg*(tp: Base.Type): BOOLEAN;
	RETURN (tp.size = 1) & (tp.size = 2) & (tp.size = 4) & (tp.size = 8)
END FitInReg;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MakeItem*(VAR x: Base.Item; obj: Base.Object);
END MakeItem;

PROCEDURE MakeConst*(VAR x: Base.Item; tp: Base.Type; val: INTEGER);
END MakeConst;

PROCEDURE MakeStr*(
	VAR x: Base.Item; str: Base.String; slen: INTEGER; ansi: BOOLEAN
);
END MakeStr;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE LoadVolatile*(VAR x: Base.Item);
END LoadVolatile;

PROCEDURE Negate*(VAR x: Base.Item);
END Negate;

PROCEDURE Not*(VAR x: Base.Item);
END Not;

PROCEDURE IntAdd*(VAR x, y: Base.Item; op: INTEGER);
END IntAdd;

PROCEDURE IntMul*(VAR x, y: Base.Item);
END IntMul;

PROCEDURE IntDiv*(VAR x, y: Base.Item; div: BOOLEAN);
END IntDiv;

PROCEDURE RealOp*(VAR x, y: Base.Item; op: INTEGER);
END RealOp;

PROCEDURE SetOp*(VAR x, y: Base.Item; op: INTEGER);
END SetOp;

PROCEDURE Or1*(VAR x: Base.Item);
END Or1;

PROCEDURE Or2*(VAR x, y: Base.Item);
END Or2;

PROCEDURE And1*(VAR x: Base.Item);
END And1;

PROCEDURE And2*(VAR x, y: Base.Item);
END And2;

PROCEDURE Compare*(VAR x, y: Base.Item; rel: INTEGER);
END Compare;

PROCEDURE RealCompare*(VAR x, y: Base.Item; rel: INTEGER);
END RealCompare;

PROCEDURE StrCompare*(VAR x, y: Base.Item; rel: INTEGER);
END StrCompare;

PROCEDURE Membership*(VAR x, y: Base.Item);
END Membership;

PROCEDURE TypeTest*(VAR x: Base.Item; tp: Base.Type; guard: BOOLEAN);
END TypeTest;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Deref*(VAR x: Base.Item);
END Deref;

PROCEDURE Field*(VAR x: Base.Item; field: Base.Object);
END Field;

PROCEDURE Index*(VAR x, idx: Base.Item);
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Call*(VAR x: Base.Item);
END Call;

PROCEDURE ReturnValue*(VAR x: Base.Item);
END ReturnValue;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Enter*;
END Enter;

PROCEDURE Return*;
END Return;
	
PROCEDURE Init*;
END Init;

PROCEDURE Finish*;
END Finish;

END Generator0.