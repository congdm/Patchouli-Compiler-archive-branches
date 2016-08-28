MODULE Parser1;

IMPORT
	B := Base1, S := Scanner1;
	
TYPE
	UndefPtrList = POINTER TO RECORD
		name: B.IdStr; tp: B.PointerType; next: UndefPtrList
	END;
	
VAR
	sym: INTEGER;
	undefList: UndefPtrList;
	type0: PROCEDURE(): B.Type;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE GetSym;
BEGIN S.Get(sym)
END GetSym;

PROCEDURE Mark(msg: ARRAY OF CHAR);
BEGIN S.Mark(msg)
END Mark;

PROCEDURE Check0(expect: INTEGER);
BEGIN
	IF sym = expect THEN GetSym
	ELSIF expect = S.semicolon THEN Mark('No ;')
	ELSIF expect = S.eql THEN Mark('No =')
	ELSIF expect = S.colon THEN Mark('No :')
	ELSIF expect = S.of THEN Mark('No OF')
	ELSIF expect = S.end THEN Mark('No END')
	ELSIF expect = S.to THEN Mark('No TO')
	ELSIF expect = S.rparen THEN Mark('No )')
	ELSE ASSERT(FALSE)
	END;
END Check0;

PROCEDURE Missing(s: INTEGER);
BEGIN
	IF s = S.ident THEN Mark('No ident?')
	ELSE ASSERT(FALSE)
	END
END Missing;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewIdent(name: B.IdStr): B.Ident;
	VAR ident, p: B.Ident;
BEGIN
	NEW(ident); ident.name := name;
	IF B.topScope.first = NIL THEN B.topScope.first := ident
	ELSE p := B.topScope.first;
		WHILE (p.next # NIL) & (p.name # name) DO p := p.next END;
		IF p.name = name THEN Mark('Ident already used'); ident := NIL END
	END;
	RETURN ident
END NewIdent;

PROCEDURE qualident(): B.Object;
	RETURN NIL
END qualident;

PROCEDURE expression(): B.Object;
	VAR x: B.Const;
BEGIN
	NEW(x); x.isType := FALSE; x.type := B.intType; x.val := 0
END expression;

PROCEDURE ConstExpression(): B.Const;
	VAR x: B.Object; c: B.Const;
BEGIN x := expression();
	IF (x IS B.Const) OR (x IS B.Var) & (x.type.form = B.tStr) THEN
		c := x(B.Const)
	ELSE Mark('not const'); c := B.NewConst(B.intType, 0)
	END;
	RETURN c
END ConstExpression;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FormalType(): B.Type;
	VAR x: B.Object; tp: B.Type;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF (x # NIL) & x.isType THEN tp := x.type
		ELSE Mark('not type')
		END
	ELSIF sym = S.array THEN
		tp := B.NewArray(0); GetSym; Check0(S.of);
		IF sym = S.array THEN Mark('Multi-dim open array not supported') END;
		tp(B.ArrayType).base := FormalType()
	END;
	RETURN tp
END FormalType;

PROCEDURE FPSection(proc: B.ProcType);
	VAR ref, ronly: BOOLEAN;
		first, ident: B.Ident; tp: B.Type;
BEGIN ref := FALSE;
	IF sym = S.var THEN ref := TRUE; GetSym END;
	IF sym = S.ident THEN
		first := NewIdent(S.id); GetSym;
		WHILE sym = S.comma DO GetSym;
			IF sym = S.ident THEN
				ident := NewIdent(S.id); GetSym;
				IF first = NIL THEN first := ident END
			ELSE Mark('remove ,')
			END
		END
	ELSE Mark('No params?')
	END;
	Check0(S.colon); tp := FormalType();
	ronly := (tp.form = B.tArray) OR (tp.form = B.tRec); ident := first;
	WHILE ident # NIL DO
		ident.obj := B.NewPar(proc, ref, ronly, tp); ident := ident.next
	END
END FPSection;

PROCEDURE FormalParameters(proc: B.ProcType);
	VAR ident: B.Ident; x: B.Object;
BEGIN GetSym;
	IF (sym = S.ident) OR (sym = S.var) THEN
		B.OpenScope; FPSection(proc);
		WHILE sym = S.semicolon DO GetSym;
			IF (sym = S.ident) OR (sym = S.var) THEN FPSection(proc)
			ELSE Mark('param section?')
			END
		END;
		proc.fpar := B.topScope.first; B.CloseScope
	END;
	Check0(S.rparen);
	IF sym = S.colon THEN GetSym; x := qualident();
		IF (x # NIL) & x.isType THEN
			IF ~(x.type.form IN {B.tArray, B.tRec}) THEN proc.rtype := x.type
			ELSE Mark('invalid type')
			END
		ELSE Mark('not type')
		END
	END
END FormalParameters;

PROCEDURE PointerType(defobj: B.Object): B.PointerType;
	VAR ptrType: B.PointerType; ident: B.Ident; x: B.Object; t: B.Type;
		undef: UndefPtrList;
BEGIN
	ptrType := B.NewPointer(); GetSym; Check0(S.to);
	IF defobj # NIL THEN defobj.type := ptrType END;
	IF sym = S.ident THEN ident := B.universe.first;
		WHILE (ident # NIL) & (ident.name # S.id) DO
			ident := ident.next
		END;
		IF ident # NIL THEN x := ident.obj;
			IF x.isType & (x.type.form = B.tRec) THEN
				ptrType.base := x.type(B.RecordType)
			ELSE Mark('not record type')
			END
		ELSIF B.curLev = 0 THEN
			NEW(undef); undef.tp := ptrType; undef.name := S.id;
			undef.next := undefList; undefList := undef
		ELSE Mark('not found, must be global type')
		END;
		GetSym
	ELSIF sym = S.record THEN t := type0(); ptrType.base := t(B.RecordType)
	ELSE Mark('base type?')
	END
END PointerType;

PROCEDURE FieldList(rec: B.RecordType);
	VAR first, field: B.Ident;
		ft: B.Type;
BEGIN
	first := NewIdent(S.id); GetSym;
	WHILE sym = S.comma DO GetSym;
		IF sym = S.ident THEN
			field := NewIdent(S.id); GetSym;
			IF first = NIL THEN first := field END
		ELSIF sym < S.ident THEN Missing(S.ident)
		ELSE Mark('remove ,')
		END
	END;
	Check0(S.colon); ft := type0(); field := first;
	WHILE field # NIL DO
		field.obj := B.NewField(rec, ft); field := field.next
	END;
END FieldList;

PROCEDURE BaseType(): B.RecordType;
	VAR btype: B.RecordType; p: B.PointerType; x: B.Object;
BEGIN
	IF sym = S.ident THEN x := qualident();
		IF x # NIL THEN
			IF x.isType & (x.type.form = B.tRec) THEN
				btype := x.type(B.RecordType)
			ELSIF x.isType & (x.type.form = B.tPtr) THEN
				p := x.type(B.PointerType);
				IF p.base # NIL THEN btype := p.base
				ELSE Mark('this type is not defined yet')
				END
			ELSE Mark('not record type')
			END;
			IF (btype # NIL) & (btype.lev >= B.MaxExt) THEN
				Mark('max extension limit reached'); btype := NIL
			END
		END
	END;
	RETURN btype
END BaseType;

PROCEDURE length(): INTEGER;
	VAR x: B.Const; len: INTEGER;
BEGIN x := ConstExpression(); len := 1;
	IF x.type.form = B.tInt THEN len := x.val ELSE Mark('not int') END;
	RETURN len
END length;

PROCEDURE type(): B.Type;
	VAR tp, t: B.Type; x: B.Object; arrType, atyp: B.ArrayType;
		recType: B.RecordType; ptrType: B.PointerType;
		ident: B.Ident; len: INTEGER;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF (x # NIL) & x.isType THEN tp := x.type ELSE Mark('not type') END
	ELSIF sym = S.array THEN
		GetSym; len := length(); arrType := B.NewArray(len);
		atyp := arrType; tp := arrType;
		WHILE sym = S.comma DO GetSym;
			IF sym <= S.ident THEN len := length();
				atyp.base := B.NewArray(len); atyp := atyp.base(B.ArrayType)
			ELSE Mark('remove ,')
			END
		END;
		Check0(S.of); atyp.base := type();
		B.CalculateArraySize(arrType, atyp)
	ELSIF sym = S.record THEN
		recType := B.NewRecord(); tp := recType; GetSym;
		IF sym = S.lparen THEN
			GetSym; recType.base := BaseType(); Check0(S.rparen);
			IF recType.base # NIL THEN B.ExtendRecord(recType) END
		END;
		B.OpenScope;
		IF sym = S.ident THEN FieldList(recType);
			WHILE sym = S.semicolon DO GetSym;
				IF sym = S.ident THEN FieldList(recType);
				ELSE Mark('no fieldlist, remove ;')
				END
			END
		END;
		recType.fields := B.topScope.first; B.CloseScope; Check0(S.end)
	ELSIF sym = S.pointer THEN
		tp := PointerType(NIL)
	ELSIF sym = S.procedure THEN
		GetSym; tp := B.NewProcType();
		IF sym = S.lparen THEN FormalParameters(tp(B.ProcType)) END
	ELSE Mark('no type?')
	END;
	RETURN tp
END type;

PROCEDURE DeclarationSequence(VAR varblksize: INTEGER);
	VAR first, ident: B.Ident; x: B.Object; tp: B.Type;
BEGIN
	IF sym = S.const THEN GetSym;
		WHILE sym = S.ident DO
			ident := NewIdent(S.id); GetSym; Check0(S.eql);
			x := ConstExpression(); IF ident # NIL THEN ident.obj := x END;
			Check0(S.semicolon)
		END
	END;
	IF sym = S.type THEN GetSym; undefList := NIL;
		WHILE sym = S.ident DO
			ident := NewIdent(S.id); x := NIL;
			IF ident # NIL THEN NEW(x); x.isType := FALSE; ident.obj := x END;
			GetSym; Check0(S.eql);
			IF (sym # S.pointer) OR (x = NIL) THEN tp := type();
				IF x # NIL THEN x.type := tp; x.isType := TRUE END
			ELSE x.isType := TRUE; x.type := PointerType(x)
			END;
			Check0(S.semicolon)
		END
	END;
	IF sym = S.var THEN GetSym;
		WHILE sym = S.ident DO
			first := NewIdent(S.id); GetSym;
			WHILE sym = S.comma DO GetSym;
				IF sym = S.ident THEN ident := NewIdent(S.id)
				ELSE Missing(S.ident); ident := NIL
				END;
				IF first = NIL THEN first := ident END; GetSym
			END;
			Check0(S.colon); tp := type(); ident := first;
			WHILE ident # NIL DO
				ident.obj := B.NewVar(tp, varblksize); ident := ident.next
			END;
		END
	END;
END DeclarationSequence;

PROCEDURE Module*;
	VAR modid: B.IdStr;
		varblksize: INTEGER;
BEGIN GetSym; modid[0] := 0X;
	IF sym = S.ident THEN modid := S.id; GetSym ELSE Missing(S.ident) END;
	Check0(S.semicolon); DeclarationSequence(varblksize);
END Module;
	
BEGIN
	type0 := type
END Parser1.