MODULE Base0;

CONST
	WordSize* = 8; CharSize* = 2; MaxChar* = 65535; SetUpperLimit* = 64;
	MaxInt* = 9223372036854775807; MinInt* = -MaxInt - 1;
	MaxIdLen* = 63; MaxStrLen* = 255;
	MaxExtension* = 8; MaxRecordTypes* = 512;
	MaxModules* = 256; MaxExportTypes* = 1024;
	
	(* Object class/Item mode *)
	cHead* = 0; cModule* = 1; cVar* = 2; cRef* = 3; cConst* = 4;
	cField* = 5; cType* = 6; cProc* = 7; cSProc* = 8; cSFunc* = 9;
	mReg* = 10; mRegI* = 11; mCond* = 12; mXreg* = 13;
	
	clsVariable* = {cVar, cRef, mRegI};
	clsValue* = clsVariable	+ {cConst, mReg, mCond, cProc, mXreg};

	(* Type form *)
	tInteger* = 0; tBoolean* = 1; tSet* = 2; tChar* = 3;
	tReal* = 4; tPointer* = 5; tProcedure* = 6;
	tArray* = 7; tRecord* = 8; tString* = 9; tNil* = 10;
	tNPointer* = 11; tNRecord* = 12;	

TYPE
	IdStr* = ARRAY MaxIdLen+1 OF CHAR;
	
	Type* = POINTER TO TypeDesc;
	TypeDesc* = RECORD
		form*, len*, size*: INTEGER;
		nopar*, parblksize*: INTEGER;
		fields*: Object;
		base*: Type
	END;
	
	Object* = POINTER TO ObjectDesc;
	ObjectDesc* = RECORD
		readOnly*: BOOLEAN;
		type*: Type;
		dsc*, next*: Object;
		val*, lev*, class*: INTEGER
	END;
	
	Item* = RECORD
		readOnly*: BOOLEAN;
		type*: Type;
		a*, b*, c*, r*: INTEGER;
		mode*, lev*: INTEGER
	END;
	
VAR
	guard*: Object;

END Base0.