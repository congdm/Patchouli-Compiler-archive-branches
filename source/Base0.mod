MODULE Base0;

CONST
	WordSize* = 8; CharSize* = 2; MaxChar* = 65535; MaxSet* = 64;
	MaxInt* = 9223372036854775807; MinInt* = -MaxInt - 1;
	MaxIdLen* = 63; MaxStrLen* = 255;
	MaxExt* = 8; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024;
	
	(* Object class/Item mode *)
	cHead* = 0; cModule* = 1; cVar* = 2; cRef* = 3; cConst* = 4;
	cField* = 5; cType* = 6; cProc* = 7; cSProc* = 8; cSFunc* = 9;
	mReg* = 10; mRegI* = 11; mCond* = 12; mXreg* = 13;
	
	clsVariable* = {cVar, cRef, mRegI};
	clsValue* = clsVariable	+ {cConst, mReg, mCond, cProc, mXreg};

	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3;
	tReal* = 4; tPtr* = 5; tProc* = 6;
	tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;
	
	typEql* = {tBool, tSet, tPtr, tProc, tNil};

TYPE
	IdStr* = ARRAY MaxIdLen+1 OF CHAR;
	String* = ARRAY MaxStrLen+1 OF CHAR;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjectDesc;
	
	TypeDesc* = RECORD
		form*, len*, size*, align*: INTEGER;
		nopar*, parblksize*: INTEGER;
		fields*: Object;
		base*: Type
	END;
	
	ObjectDesc* = RECORD
		readOnly*: BOOLEAN;
		name*: IdStr;
		dsc*, next*: Object;
		type*: Type;
		val*, lev*, class*: INTEGER
	END;
	
	Item* = RECORD
		readOnly*: BOOLEAN;
		type*: Type;
		a*, b*, c*, r*: INTEGER;
		mode*, lev*: INTEGER
	END;
	
VAR
	intType*, realType*, charType*, boolType*, nilType*: Type;
	
PROCEDURE NewType*(VAR tp: Type; form: INTEGER);
END NewType;

END Base0.