MODULE Parser1;

IMPORT
	B := Base1, S := Scanner1, G := Generator1;
	
TYPE
	UndefPtrList = POINTER TO RECORD
		name: B.IdStr; tp: B.Type; next: UndefPtrList
	END;
	
VAR
	sym: INTEGER;
	undefList: UndefPtrList;
	curProcIdent: B.Ident;
	
	type0: PROCEDURE(): B.Type;
	expression0: PROCEDURE(): B.Object;
	
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
	ELSIF expect = S.rbrak THEN Mark('No ]')
	ELSIF expect = S.rbrace THEN Mark('No }')
	ELSE ASSERT(FALSE)
	END;
END Check0;

PROCEDURE Missing(s: INTEGER);
BEGIN
	IF s = S.ident THEN Mark('No ident?')
    ELSIF s = S.return THEN Mark('No RETURN?')
	ELSE ASSERT(FALSE)
	END
END Missing;

PROCEDURE IsStr(t: B.Type): BOOLEAN;
	RETURN (t.form = B.tStr) OR (t.form = B.tArray) & (t.base.form = B.tChar)
END IsStr;

PROCEDURE IsExt0(t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2) OR (t1.len > t2.len) & IsExt0(t1.base, t2)
END IsExt0;

PROCEDURE IsExt(t1, t2: B.Type): BOOLEAN;
BEGIN
	IF t1.form = B.tPtr THEN t1 := t1.base END;
	IF t2.form = B.tPtr THEN t2 := t2.base END;
	RETURN IsExt0(t1, t2)
END IsExt;

PROCEDURE SamePars(p1, p2: B.Ident): BOOLEAN;
	RETURN (p1 = NIL) & (p2 = NIL)
	OR (p1 # NIL) & (p2 # NIL)
		& (p1.obj(B.Var).ronly = p2.obj(B.Var).ronly)
		& (p1.obj(B.Var).ref = p2.obj(B.Var).ref)
		& ((p1.obj.type = p2.obj.type)
			OR (p1.obj.type.form = B.tArray) & (p2.obj.type.form = B.tArray)
				& (p1.obj.type.len = 0) & (p2.obj.type.len = 0)
				& (p1.obj.type.base = p2.obj.type.base))
		& SamePars(p1.next, p2.next)
END SamePars;

PROCEDURE SameProc(t1, t2: B.Type): BOOLEAN;
	RETURN (t1.base = t2.base) & (t1.nfpar = t2.nfpar)
		& (t1.parblksize = t2.parblksize) & SamePars(t1.fields, t2.fields)
END SameProc;

PROCEDURE CompTypes(t1, t2: B.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = B.tInt) & (t2.form = B.tInt)
	OR (t1.form = B.tChar) & IsStr(t2) & (t2.len <= 2)
	OR IsStr(t1) & IsStr(t2)
	OR (t1.form IN {B.tProc, B.tPtr}) & (t2 = B.nilType)
	OR (t1.form IN {B.tRec, B.tPtr}) & (t1.form = t2.form) & IsExt(t2, t1)
	OR (t1.form = B.tProc) & (t2.form = B.tProc) & SameProc(t1, t2)
END CompTypes;

PROCEDURE IsVarPar(x: B.Object): BOOLEAN;
	RETURN (x IS B.Var) & x(B.Var).ref & ~x(B.Var).ronly
END IsVarPar;

PROCEDURE IsConst(x: B.Object): BOOLEAN;
	RETURN (x IS B.Const) OR (x IS B.Var) & (x.type.form = B.tStr)
END IsConst;

PROCEDURE CheckInt(x: B.Object);
BEGIN
	IF x.type.form # B.tInt THEN Mark('not int') END
END CheckInt;

PROCEDURE CheckBool(x: B.Object);
BEGIN
	IF x.type # B.boolType THEN Mark('not int') END
END CheckBool;

PROCEDURE TypeTestable(x: B.Object): BOOLEAN;
	RETURN (x.type.form = B.tPtr) & (x.type.base # NIL)
	OR (x.type.form = B.tRec) & IsVarPar(x)
END TypeTestable;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewIdent(name: B.IdStr): B.Ident;
	VAR ident, p: B.Ident;
BEGIN
	NEW(ident); ident.name := name;
	IF B.topScope.first = NIL THEN B.topScope.first := ident
	ELSE p := B.topScope.first;
		WHILE (p.next # NIL) & (p.name # name) DO p := p.next END;
		IF p.name = name THEN Mark('Ident already used'); ident := NIL
		ELSE p.next := ident
		END
	END;
	RETURN ident
END NewIdent;

PROCEDURE NewNode(op: INTEGER; x, y: B.Object): B.Node;
	VAR z: B.Node;
BEGIN
	NEW(z); z.isType := FALSE; z.op := op; z.left := x; z.right := y;
	RETURN z
END NewNode;

PROCEDURE qualident(): B.Object;
	VAR x: B.Object; ident: B.Ident;
BEGIN ident := B.topScope.first;
	WHILE (ident # NIL) & (ident.name # S.id) DO ident := ident.next END;
	IF ident # NIL THEN x := ident.obj
	ELSIF (curProcIdent # NIL) & (S.id = curProcIdent.name) THEN
		x := curProcIdent.obj
	ELSE ident := B.universe.first;
		WHILE (ident # NIL) & (ident.name # S.id) DO ident := ident.next END;
		IF ident # NIL THEN x := ident.obj
		ELSE Mark('identifier not found')
		END
	END;
	RETURN x
END qualident;

PROCEDURE designator(): B.Object;
	VAR x, y: B.Object; id: B.IdStr; fld: B.Ident;
		recType, xtype, ytype: B.Type;
		xform, yform: INTEGER; valid: BOOLEAN;
BEGIN x := qualident(); valid := TRUE;
	IF x = NIL THEN x := B.NewConst(B.intType, 0); valid := FALSE
	ELSIF x.isType THEN Mark('invalid value');
		x := B.NewConst(B.intType, 0); valid := FALSE
	END;
	WHILE sym = S.period DO
		xform := x.type.form;
		IF valid THEN
			IF xform = B.tPtr THEN
				IF x.type.base = NIL THEN
					Mark('invalid pointer'); valid := FALSE
				END
			ELSIF xform # B.tRec THEN
				Mark('not pointer or record'); valid := FALSE
			END 
		END;
		GetSym;
		IF valid & (sym # S.ident) THEN
			Mark('No record field?'); valid := FALSE
		ELSIF sym = S.ident THEN GetSym
		END;
		IF valid THEN id := S.id;
			IF xform = B.tPtr THEN recType := x.type.base
			ELSE recType := x.type
			END;
			REPEAT fld := recType.fields;
				WHILE (fld # NIL) & (fld.name # id) DO fld := fld.next END;
				IF fld # NIL THEN
					x := NewNode(S.period, x, fld.obj); x.type := fld.obj.type
				ELSE recType := recType.base;
					IF recType = NIL THEN Mark('Field not found') END
				END
			UNTIL (fld # NIL) OR (recType = NIL);
			GetSym
		END
	ELSIF sym = S.lbrak DO
		xtype := x.type; xform := xtype.form;
		IF valid & (xform # B.tArray) THEN
			Mark('Not an array'); valid := FALSE
		END;
		GetSym; y := expression0(); CheckInt(y);
		IF valid THEN
			x := NewNode(S.lbrak, x, y); x.type := xtype.base
		END;
		WHILE sym = S.comma DO
			xtype := x.type; xform := xtype.form;
			IF valid & (xform # B.tArray) THEN
				Mark('too many dimensions'); valid := FALSE
			END;
			GetSym; y := expression0(); CheckInt(y);
			IF valid THEN
				x := NewNode(S.lbrak, x, y); x.type := xtype.base
			END
		END;
		Check0(S.rbrak)
	ELSIF sym = S.arrow DO
		xtype := x.type; xform := xtype.form;
		IF valid & (xform # B.tPtr) THEN
			Mark('Not a pointer'); valid := FALSE
		END;
		IF valid THEN
			x := NewNode(S.arrow, x, NIL); x.type := xtype.base
		END;
		GetSym
	ELSIF (sym = S.lparen) & TypeTestable(x) DO
		xtype := x.type; xform := xtype.form; GetSym;
		IF sym = S.ident THEN y := qualident()
		ELSIF valid THEN Missing(S.ident); valid := FALSE
		END;
		IF valid THEN
			IF y = NIL THEN valid := FALSE
			ELSIF ~y.isType THEN Mark('not type'); valid := FALSE
			ELSE ytype := y.type; yform := ytype.form 
			END
		END;
		IF valid THEN
			IF (xform = yform) & IsExt(ytype, xtype) THEN
				x := NewNode(S.lparen, x, y); x.type := ytype
			ELSIF (xform = B.tRec) & IsExt(ytype, xtype) THEN
				x := NewNode(S.lparen, x, y); x.type := ytype.base
			ELSE Mark('invalid type'); valid := FALSE
			END
		END;
		Check0(S.rparen)
	END;
	RETURN x
END designator;

PROCEDURE element(): B.Object;
	VAR x, y: B.Object;
BEGIN
	x := expression0(); CheckInt(x);
	IF sym = S.upto THEN
		y := expression0(); CheckInt(y); x := NewNode(S.upto, x, y)
	END;
	RETURN x
END element;

PROCEDURE set(): B.Object;
	VAR x, t, u: B.Node;
BEGIN x := NewNode(S.lbrace, NIL, NIL); GetSym;
	IF sym # S.rbrace THEN
		x.left := element(); t := x;
		WHILE sym = S.comma DO
			IF sym # S.rbrace THEN
				u := NewNode(S.comma, NIL, NIL); t.right := u; t := u;
				t.left := element()
			ELSE Mark('remove ,')
			END
		END
	END;
	Check0(S.rbrace);
	RETURN x
END set;

PROCEDURE factor(): B.Object;
	VAR x: B.Object;
BEGIN
	IF sym = S.int THEN x := B.NewConst(B.intType, S.ival)
	ELSIF sym = S.real THEN x := B.NewConst(B.realType, S.ival)
	ELSIF sym = S.string THEN x := B.NewStr(S.str, S.slen)
	ELSIF sym = S.nil THEN x := B.NewConst(B.nilType, 0)
	ELSIF sym = S.true THEN x := B.NewConst(B.boolType, 1)
	ELSIF sym = S.false THEN x := B.NewConst(B.boolType, 0)
	ELSIF sym = S.lbrace THEN x := set()
	ELSIF sym = S.ident THEN x := designator() (*stub*)
	ELSIF sym = S.lparen THEN GetSym; x := expression0(); Check0(S.rparen)
	ELSIF sym = S.not THEN GetSym; x := factor(); CheckBool(x);
		x := NewNode(S.not, x, NIL); x.type := B.boolType
	ELSE Mark('Invalid factor'); x := B.NewConst(B.intType, 0)
	END;
	RETURN x
END factor;

PROCEDURE term(): B.Object;
	VAR x, y: B.Object; xtype: B.Type;
		xform, op: INTEGER; errflag: BOOLEAN;
BEGIN
	x := factor(); xform := x.type.form;
	WHILE sym = S.times DO
		xtype := x.type; xform := xtype.form;
		IF xform IN {B.tInt, B.tReal, B.tSet} THEN (*valid*)
		ELSE Mark('invalid type'); errflag := TRUE
		END;
		op := sym; GetSym; y := factor();
		IF (y.type = xtype) OR (xform = B.tInt) & (y.type.form = B.tInt)
		THEN (*valid*) ELSE Mark('invalid type'); errflag := TRUE
		END;
		x := NewNode(op, x, y);
		IF ~errflag THEN
			IF xform = B.tInt THEN x.type := B.intType
			ELSE x.type := xtype
			END
		ELSE x.type := B.intType
		END
	ELSIF sym = S.rdiv DO
		xtype := x.type;
		IF xtype.form IN {B.tReal, B.tSet} THEN (*valid*)
		ELSE Mark('invalid type'); errflag := TRUE
		END;
		op := sym; GetSym; y := factor();
		IF y.type = xtype THEN (*valid*)
		ELSE Mark('invalid type'); errflag := TRUE
		END;
		x := NewNode(op, x, y);
		IF ~errflag THEN x.type := xtype
		ELSE x.type := B.realType
		END
	ELSIF (sym = S.div) OR (sym = S.mod) DO
		CheckInt(x); op := sym; GetSym; y := factor(); CheckInt(y);
		x := NewNode(op, x, y); x.type := B.intType
	ELSIF sym = S.and DO
		CheckBool(x); GetSym; y := factor(); CheckBool(y);
		x := NewNode(S.and, x, y); x.type := B.boolType
	END;
	RETURN x
END term;

PROCEDURE SimpleExpression(): B.Object;	
	VAR x, y: B.Object; xtype: B.Type;
		xform, op: INTEGER; errflag: BOOLEAN;
BEGIN
	IF sym = S.plus THEN GetSym; x := term()
	ELSIF sym = S.minus THEN GetSym; x := term();
		xtype := x.type; x := NewNode(S.minus, x, NIL);
		IF (xtype.form IN {B.tInt, B.tReal, B.tSet}) THEN x.type := xtype
		ELSE Mark('invalid type'); x.type := B.intType
		END
	ELSE x := term()
	END;
	WHILE (sym = S.plus) OR (sym = S.minus) DO
		xtype := x.type; xform := xtype.form;
		IF xform IN {B.tInt, B.tReal, B.tSet} THEN (*valid*)
		ELSE Mark('invalid type'); errflag := TRUE
		END;
		op := sym; GetSym; y := term();
		IF (y.type = xtype) OR (xform = B.tInt) & (y.type.form = B.tInt)
		THEN (*valid*) ELSE Mark('invalid type'); errflag := TRUE
		END;
		x := NewNode(op, x, y);
		IF ~errflag THEN
			IF xform = B.tInt THEN x.type := B.intType
			ELSE x.type := xtype
			END
		ELSE x.type := B.intType
		END
	ELSIF sym = S.or DO
		CheckBool(x); GetSym; y := term(); CheckBool(y);
		x := NewNode(S.or, x, y); x.type := B.boolType
	END;
	RETURN x
END SimpleExpression;

PROCEDURE expression(): B.Object;
	VAR x, y: B.Object; tp: B.Type;
		xform, yform, op: INTEGER; errflag: BOOLEAN;
BEGIN
	x := SimpleExpression(); errflag := FALSE; xform := x.type.form;
	IF (sym >= S.eql) & (sym <= S.in) THEN
		IF sym <= S.neq THEN
			IF (xform IN B.typEql + B.typCmp) OR IsStr(x.type) THEN (*valid*)
			ELSE Mark('invalid type'); errflag := TRUE
			END;
			op := sym; GetSym; y := SimpleExpression(); yform := y.type.form;
			IF ~CompTypes(x.type, y.type) & ~CompTypes(y.type, x.type) THEN
				Mark('invalid expression'); errflag := TRUE
			END
		ELSIF (sym >= S.lss) & (sym <= S.geq) THEN
			IF (xform IN B.typCmp) OR IsStr(x.type) THEN (*valid*)
			ELSE Mark('invalid type'); errflag := TRUE;
			END;
			op := sym; GetSym; y := SimpleExpression(); yform := y.type.form;
			IF ~CompTypes(x.type, y.type) & ~CompTypes(y.type, x.type) THEN
				Mark('invalid expression'); errflag := TRUE
			END
		ELSIF sym = S.in THEN
			IF xform # B.tInt THEN Mark('invalid type'); errflag := TRUE END;
			op := sym; GetSym; y := SimpleExpression();
			IF y.type.form # B.tSet THEN
				Mark('invalid expression'); errflag := TRUE
			END
		END;
		IF ~errflag THEN
			x := NewNode(op, x, y); x.type := B.boolType
		ELSE x := B.NewConst(B.boolType, 0)
		END
	ELSIF sym = S.is THEN
		IF TypeTestable(x) THEN (*valid*)
		ELSE Mark('invalid type'); errflag := TRUE
		END;
		GetSym;
		IF sym = S.ident THEN y := qualident()
		ELSE Missing(S.ident); errflag := TRUE
		END;
		IF ~errflag THEN
			IF (y # NIL) & y.isType THEN tp := y.type;
				IF (tp.form = B.tPtr) & (tp.base # NIL) & IsExt(tp, x.type)
				OR (tp.form = B.tRec) & IsExt(tp, x.type)
				THEN (*valid*) ELSE Mark('invalid type'); errflag := TRUE
				END;
			ELSE Mark('not type'); errflag := TRUE
			END
		END;
		IF ~errflag THEN x := NewNode(S.is, x, y); x.type := B.boolType
		ELSE x := B.NewConst(B.boolType, 0)
		END
	END;
	RETURN x
END expression;

PROCEDURE ConstExpression(): B.Object;
	VAR x: B.Object;
BEGIN x := expression();
	IF (x IS B.Const) OR (x IS B.Var) & (x.type.form = B.tStr) THEN (*valid*)
	ELSE Mark('not const'); x := B.NewConst(B.intType, 0)
	END;
	RETURN x
END ConstExpression;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE StatementSequence(): B.Node;
BEGIN
    RETURN NIL
END StatementSequence;

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
		tp.base := FormalType()
	END;
	RETURN tp
END FormalType;

PROCEDURE FPSection(proc: B.Type);
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

PROCEDURE FormalParameters(proc: B.Type);
	VAR ident: B.Ident; x: B.Object;
BEGIN GetSym;
	IF (sym = S.ident) OR (sym = S.var) THEN
		B.OpenScope; FPSection(proc);
		WHILE sym = S.semicolon DO GetSym;
			IF (sym = S.ident) OR (sym = S.var) THEN FPSection(proc)
			ELSE Mark('param section?')
			END
		END;
		proc.fields := B.topScope.first; B.CloseScope
	END;
	Check0(S.rparen);
	IF sym = S.colon THEN GetSym;
		IF sym = S.ident THEN x := qualident()
		ELSE Missing(S.ident); x := NIL
		END;
		IF (x # NIL) & x.isType THEN
			IF ~(x.type.form IN {B.tArray, B.tRec}) THEN proc.base := x.type
			ELSE Mark('invalid type')
			END
		ELSE Mark('not type')
		END
	END
END FormalParameters;

PROCEDURE PointerType(defobj: B.Object): B.Type;
	VAR ptrType: B.Type; ident: B.Ident; x: B.Object;
		undef: UndefPtrList;
BEGIN
	ptrType := B.NewPointer(); GetSym; Check0(S.to);
	IF defobj # NIL THEN defobj.type := ptrType END;
	IF sym = S.ident THEN ident := B.universe.first;
		WHILE (ident # NIL) & (ident.name # S.id) DO ident := ident.next END;
		IF ident # NIL THEN x := ident.obj;
			IF x.isType & (x.type.form = B.tRec) THEN ptrType.base := x.type
			ELSE Mark('not record type')
			END
		ELSIF B.curLev = 0 THEN
			NEW(undef); undef.tp := ptrType; undef.name := S.id;
			undef.next := undefList; undefList := undef
		ELSE Mark('not found, must be global type')
		END;
		GetSym
	ELSIF sym = S.record THEN ptrType.base := type0()
	ELSE Mark('base type?')
	END
END PointerType;

PROCEDURE FieldList(rec: B.Type);
	VAR first, field: B.Ident; ft: B.Type;
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

PROCEDURE BaseType(): B.Type;
	VAR btype, p: B.Type; x: B.Object;
BEGIN
	IF sym = S.ident THEN x := qualident();
		IF x # NIL THEN
			IF x.isType & (x.type.form = B.tRec) THEN btype := x.type
			ELSIF x.isType & (x.type.form = B.tPtr) THEN p := x.type;
				IF p.base # NIL THEN btype := p.base
				ELSE Mark('this type is not defined yet')
				END
			ELSE Mark('not record type')
			END;
			IF (btype # NIL) & (btype.len >= B.MaxExt) THEN
				Mark('max extension limit reached'); btype := NIL
			END
		END
	ELSE Missing(S.ident)
	END;
	RETURN btype
END BaseType;

PROCEDURE length(): INTEGER;
	VAR x: B.Object; len: INTEGER;
BEGIN x := ConstExpression(); len := 1;
	IF x.type.form = B.tInt THEN len := x(B.Const).val ELSE Mark('not int') END;
	RETURN len
END length;

PROCEDURE type(): B.Type;
	VAR tp, lastArr, t: B.Type; x: B.Object; proc: B.Proc;
		ident: B.Ident; len: INTEGER;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF (x # NIL) & x.isType THEN tp := x.type ELSE Mark('not type') END
	ELSIF sym = S.array THEN
		GetSym; len := length(); tp := B.NewArray(len); lastArr := tp;
		WHILE sym = S.comma DO GetSym;
			IF sym <= S.ident THEN len := length();
				lastArr.base := B.NewArray(len); lastArr := lastArr.base
			ELSE Mark('remove ,')
			END
		END;
		Check0(S.of); lastArr.base := type(); B.CalculateArraySize(tp, lastArr)
	ELSIF sym = S.record THEN
		tp := B.NewRecord(); GetSym;
		IF sym = S.lparen THEN
			GetSym; tp.base := BaseType(); Check0(S.rparen);
			IF tp.base # NIL THEN B.ExtendRecord(tp) END
		END;
		B.OpenScope;
		IF sym = S.ident THEN FieldList(tp);
			WHILE sym = S.semicolon DO GetSym;
				IF sym = S.ident THEN FieldList(tp);
				ELSE Mark('no fieldlist, remove ;')
				END
			END
		END;
		tp.fields := B.topScope.first; B.CloseScope; Check0(S.end)
	ELSIF sym = S.pointer THEN
		tp := PointerType(NIL)
	ELSIF sym = S.procedure THEN
		GetSym; tp := B.NewProcType();
		IF sym = S.lparen THEN FormalParameters(tp) END
	ELSE Mark('no type?')
	END;
	RETURN tp
END type;

PROCEDURE DeclarationSequence(VAR varblksize: INTEGER);
	VAR first, ident, par: B.Ident; x: B.Object; tp: B.Type; varobj: B.Var;
        locblksize: INTEGER; statseq: B.Node;
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
    WHILE sym = S.procedure DO GetSym;
        IF sym = S.ident THEN
            curProcIdent := NewIdent(S.id); GetSym;
            proc := B.NewProc(); ident.obj := proc
        ELSE curProcIdent := NIL; Mark('proc name?')
        END;
        tp := B.NewProcType(); IF sym = S.lparen THEN FormalParameters(tp) END;
        Check0(S.semicolon); IF proc # NIL THEN proc.type := tp END;
        B.OpenScope; par := tp.fields;
        WHILE par # NIL DO
            ident := NewIdent(par.name); NEW(varobj); ident.obj := varobj;
            varobj^ := par.obj(B.Var)^; par := par.next
        END;
        ident := curProcIdent; locblksize := 0;
        DeclarationSequence(locblksize); curProcIdent := ident;
        IF proc # NIL THEN
            proc.decl := B.topScope.first; proc.locblksize := locblksize
        END;
        IF sym = S.begin THEN
            GetSym; statseq := StatementSequence()
        ELSE statseq := NIL
        END;
        IF sym = S.return THEN
            IF tp.base = NIL THEN Mark('not function proc') END;
            return := expression();
            IF return.type.form IN {B.tArray, B.tRec} THEN
                Mark('invalid type')
            END                
        ELSE return := NIL; IF tp.base # NIL THEN Missing(S.return) END
        END;
        IF proc # NIL THEN
            proc.statseq := statseq; proc.return := return
        END;
        Check0(S.end);
        IF sym = S.ident THEN
            IF (curProcIdent # NIL) & (curProcIdent.name # S.id) THEN
                Mark('wrong proc ident')
            END;
            GetSym
        ELSIF curProcIdent # NIL THEN Missing(S.ident)
        END;
        Check0(S.semicolon)
    END
END DeclarationSequence;

PROCEDURE Module*;
	VAR modid: B.IdStr;
		varblksize: INTEGER;
BEGIN GetSym; modid[0] := 0X;
	IF sym = S.ident THEN modid := S.id; GetSym ELSE Missing(S.ident) END;
	Check0(S.semicolon); DeclarationSequence(varblksize);
	IF errcnt = 0 THEN
	END
END Module;
	
BEGIN
	type0 := type; expression0 := expression
END Parser1.