MODULE Generator0;

IMPORT
	Base := Base0,
	Scanner,
	SymTable := SymTable0;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE LoadVolatile*(VAR x: Base.Item);
END LoadVolatile;

PROCEDURE Negate*(VAR x: Base.Item);
END Negate;

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

PROCEDURE StrCompare*(VAR x, y: Base.Item; rel: INTEGER);
END StrCompare;

PROCEDURE Membership*(VAR x, y: Base.Item);
END Membership;

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