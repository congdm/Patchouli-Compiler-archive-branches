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
	StatementSequence0: PROCEDURE(): B.Node;
	
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
	ELSIF expect = S.then THEN Mark('No THEN')
	ELSIF expect = S.do THEN Mark('No DO')
	ELSIF expect = S.until THEN Mark('No UNTIL')
	ELSIF expect = S.becomes THEN Mark('No :=')
	ELSIF expect = S.period THEN Mark('No .')
	ELSE ASSERT(FALSE)
	END;
END Check0;

PROCEDURE Missing(s: INTEGER);
BEGIN
	IF s = S.ident THEN Mark('No ident?')
	ELSIF s = S.return THEN Mark('No RETURN?')
	ELSIF s = S.comma THEN Mark('no ,')
	ELSIF s = S.semicolon THEN Mark('no ;')
	ELSE ASSERT(FALSE)
	END
END Missing;

PROCEDURE IsStr(t: B.Type): BOOLEAN;
	RETURN (t = B.strType) OR (t.form = B.tArray) & (t.base.form = B.tChar)
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

PROCEDURE IsVarPar(x: B.Object): BOOLEAN;
	RETURN (x IS B.Var) & x(B.Var).par & (x.class = B.cRef)
END IsVarPar;

PROCEDURE IsConst(x: B.Object): BOOLEAN;
	RETURN (x IS B.Const) OR (x IS B.Str)
END IsConst;

PROCEDURE IsOpenArray(tp: B.Type): BOOLEAN;
	RETURN (tp.form = B.tArray) & (tp.len = 0)
END IsOpenArray;

PROCEDURE SamePars(p1, p2: B.Ident): BOOLEAN;
	RETURN (p1 = NIL) & (p2 = NIL)
	OR (p1 # NIL) & (p2 # NIL)
		& (p1.obj.class = p2.obj.class)
		& (p1.obj(B.Var).ronly = p2.obj(B.Var).ronly)
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

PROCEDURE CompTypes2(t1, t2: B.Type): BOOLEAN;
	RETURN CompTypes(t1, t2) OR CompTypes(t2, t1)
END CompTypes2;

PROCEDURE CheckInt(x: B.Object);
BEGIN
	IF x.type.form # B.tInt THEN Mark('not int') END
END CheckInt;

PROCEDURE CheckBool(x: B.Object);
BEGIN
	IF x.type # B.boolType THEN Mark('not bool') END
END CheckBool;

PROCEDURE CheckSet(x: B.Object);
BEGIN
	IF x.type # B.setType THEN Mark('not set') END
END CheckSet;

PROCEDURE TypeTestable(x: B.Object): BOOLEAN;
	RETURN (x.type.form = B.tPtr) & (x.type.base # NIL)
	OR (x.type.form = B.tRec) & IsVarPar(x)
END TypeTestable;

PROCEDURE CheckPar(fpar: B.Var; x: B.Object);
	VAR xtype, ftype: B.Type; xform, fform: INTEGER;
BEGIN xtype := x.type; ftype := fpar.type;
	IF fpar.class = B.cVar THEN
		IF ~CompTypes(ftype, xtype) THEN Mark('invalid type') END
	ELSIF fpar.class = B.cRef THEN
		IF x IS B.Var THEN xform := xtype.form; fform := ftype.form;
			IF x(B.Var).ronly & ~fpar.ronly THEN Mark('read only') END;
			IF (xtype = ftype)
			OR (fform = B.tRec) & (xform = B.tRec) & IsExt(xtype, ftype)
			OR IsOpenArray(ftype) & (xform = B.tArray)
				& (ftype.base = xtype.base)
			OR (fform = B.tArray) & (xform = B.tArray)
				& (ftype.base = xtype.base) & (ftype.len = xtype.len)
			OR IsOpenArray(ftype) & (ftype.base = B.byteType)
			OR IsStr(xtype) & IsStr(ftype)
			THEN (*valid*) ELSE Mark('invalid type')
			END
		ELSE Mark('not var')
		END
	END
END CheckPar;

PROCEDURE CheckLeft(x: B.Object; op: INTEGER);
	CONST ivlType = 'invalid type';
BEGIN
	IF (op >= S.eql) & (op <= S.geq) THEN
		IF (x.type.form IN B.typCmp) OR IsStr(x.type)
		OR (op <= S.neq) & (x.type.form IN B.typEql)
		THEN (*valid*) ELSE Mark(ivlType)
		END
	ELSIF op = S.is THEN
		IF TypeTestable(x) THEN (*valid*) ELSE Mark(ivlType) END
	END
END CheckLeft;

PROCEDURE Check1(x: B.Object; forms: SET);
	CONST ivlType = 'invalid type';
BEGIN
	IF ~(x.type.form IN forms) THEN Mark(ivlType) END
END Check1;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewIdent(name: B.IdStr): B.Ident;
	VAR ident, p: B.Ident;
BEGIN
	NEW(ident); ident.name := name; ident.next := NIL;
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
	NEW(z); z.class := B.cNode; z.op := op; z.left := x; z.right := y;
	RETURN z
END NewNode;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Call(x: B.Object): B.Node;
	VAR call, last: B.Node; proc: B.Proc; proctyp: B.Type;
		fpar: B.Ident; nact: INTEGER;
		
	PROCEDURE Parameter(VAR last: B.Node; fpar: B.Ident);
		VAR y: B.Object; par: B.Node;
	BEGIN y := expression0();
		IF fpar # NIL THEN CheckPar(fpar.obj(B.Var), y) END;
		par := NewNode(S.par, y, NIL); last.right := par; last := par
	END Parameter;
		
BEGIN (* Call *)
	proc := x(B.Proc); proctyp := proc.type;
	call := NewNode(S.call, proc, NIL); call.type := proctyp.base;
	IF sym = S.lparen THEN GetSym;
		IF sym # S.rparen THEN
			fpar := proctyp.fields; Parameter(last, fpar); nact := 1;
			WHILE sym = S.comma DO
				IF fpar # NIL THEN fpar := fpar.next END; GetSym;
				IF sym # S.rparen THEN Parameter(last, fpar); nact := nact + 1
				ELSE Mark('remove ,')
				END
			END;
			IF nact = proctyp.nfpar THEN (*valid*)
			ELSIF nact > proctyp.nfpar THEN Mark('too many params')
			ELSE Mark('not enough params')
			END;
			Check0(S.rparen)
		ELSIF sym = S.rparen THEN
			IF proctyp.nfpar # 0 THEN Mark('need params') END; GetSym
		END
	END;
	RETURN call
END Call;

PROCEDURE FindIdent(): B.Object;
	VAR x: B.Object; ident: B.Ident; found: BOOLEAN;
BEGIN ident := B.topScope.first; found := FALSE;
	WHILE (ident # NIL) & (ident.name # S.id) DO ident := ident.next END;
	IF ident # NIL THEN x := ident.obj; found := TRUE
	ELSIF (curProcIdent # NIL) & (S.id = curProcIdent.name) THEN
		x := curProcIdent.obj; found := TRUE
	ELSE ident := B.universe.first;
		WHILE (ident # NIL) & (ident.name # S.id) DO ident := ident.next END;
		IF ident # NIL THEN x := ident.obj; found := TRUE
		ELSE Mark('identifier not found')
		END
	END;
	IF found & (x = NIL) THEN Mark('identifier undefined') END;
	RETURN x
END FindIdent;

PROCEDURE qualident(): B.Object;
	VAR x: B.Object; ident: B.Ident;
BEGIN x := FindIdent();
	IF (x IS B.Module) & (sym = S.period) THEN GetSym;
		IF sym = S.ident THEN
			ident := x(B.Module).first;
			WHILE (ident # NIL) & (ident.name # S.id) DO
				ident := ident.next
			END;
			IF ident = NIL THEN Mark('not found'); x := NIL
			ELSE x := ident.obj
			END
		ELSE Missing(S.ident); x := NIL
		END
	ELSIF x IS B.Module THEN x := NIL
	END;
	RETURN x
END qualident;

PROCEDURE designator(): B.Object;
	VAR x, y: B.Object; fid: B.IdStr; fld: B.Ident;
		recType, xtype, ytype: B.Type;
BEGIN x := qualident();
	IF (x = NIL) OR (x.class <= B.cType) THEN
		x := B.NewConst(B.intType, 0); Mark('invalid value')
	END;
	WHILE sym = S.period DO
		Check1(x, {B.tPtr, B.tRec}); GetSym;
		IF sym # S.ident THEN Mark('no field?')
		ELSE fid := S.id;
			IF x.type.form = B.tPtr THEN recType := x.type.base
			ELSE recType := x.type
			END;
			REPEAT fld := recType.fields;
				WHILE (fld # NIL) & (fld.name # fid) DO fld := fld.next END;
				IF fld # NIL THEN y := fld.obj;
					x := NewNode(S.period, x, y); x.type := y.type
				ELSE recType := recType.base;
					IF recType = NIL THEN Mark('Field not found') END
				END
			UNTIL (fld # NIL) OR (recType = NIL);
			GetSym
		END
	ELSIF sym = S.lbrak DO
		Check1(x, {B.tArray}); GetSym; y := expression0(); CheckInt(y);
		xtype := x.type; x := NewNode(S.lbrak, x, y); x.type := xtype.base;
		IF x.type = NIL THEN x.type := xtype END;
		WHILE sym = S.comma DO
			xtype := x.type;
			IF xtype.form # B.tArray THEN Mark('too many dimensions') END;
			GetSym; y := expression0(); CheckInt(y);
			x := NewNode(S.lbrak, x, y); x.type := xtype.base;
			IF x.type = NIL THEN x.type := xtype END
		END;
		Check0(S.rbrak)
	ELSIF sym = S.arrow DO
		Check1(x, {B.tPtr}); xtype := x.type;
		x := NewNode(S.arrow, x, NIL); x.type := xtype.base;
		IF x.type = NIL THEN Mark('undefined pointer'); x.type := xtype END;
		GetSym
	ELSIF (sym = S.lparen) & TypeTestable(x) DO
		xtype := x.type; GetSym; y := NIL;
		IF sym = S.ident THEN y := qualident() ELSE Missing(S.ident) END;
		IF (y # NIL) & (y.class = B.cType) THEN ytype := y.type
		ELSE Mark('not type'); ytype := x.type
		END;
		x := NewNode(S.lparen, x, y);
		IF (xtype.form = ytype.form) & IsExt(ytype, xtype) THEN
			x.type := ytype
		ELSIF (xtype.form = B.tRec) & IsExt(ytype, xtype) THEN
			x.type := ytype.base
		ELSE Mark('invalid type'); x.type := xtype
		END;
		Check0(S.rparen)
	END;
	RETURN x
END designator;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE StdFunc(f: B.SProc): B.Node;
	VAR x, par, par2: B.Node; y: B.Object;
BEGIN
	x := NewNode(S.sproc, f, NIL); GetSym;
	IF f.id = 'ABS' THEN y := expression0(); Check1(y, {B.tInt, B.tReal});
		par := NewNode(S.par, y, NIL); x.right := par;
		IF y.type.form = B.tInt THEN x.type := B.intType
		ELSE x.type := y.type
		END
	ELSIF f.id = 'ODD' THEN y := expression0(); CheckInt(y);
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.boolType
	ELSIF f.id = 'LEN' THEN y := designator(); Check1(y, {B.tArray});
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.intType
	ELSIF (f.id = 'LSL') OR (f.id = 'ASR') OR (f.id = 'ROR') THEN
		x.type := B.intType; y := expression0(); CheckInt(y);
		par := NewNode(S.par, y, NIL); x.right := par;
		Check0(S.comma); y := expression0(); CheckInt(y);
		par2 := NewNode(S.par, y, NIL); par.right := par2
	ELSIF f.id = 'FLOOR' THEN y := expression0(); CheckReal(y);
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.intType
	ELSIF f.id = 'FLT' THEN y := expression0(); CheckInt(y);
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.realType
	ELSIF f.id = 'ORD' THEN
		y := expression0(); Check1(y, {B.tSet, B.tBool, B.tChar});
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.intType
	ELSIF f.id = 'CHR' THEN y := expression0(); CheckInt(y);
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.charType
	ELSIF f.id = 'ADR' THEN y := designator();
		IF ~((y.class >= B.cNode) & (y.class <= B.cRef)) THEN
			Mark('not var')
		END;
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.intType
	ELSIF f.id = 'SIZE' THEN y := qualident();
		IF y.class # B.cType THEN Mark('not type') END;
		par := NewNode(S.par, y, NIL); x.right := par; x.type := B.intType
	ELSIF f.id = 'BIT' THEN
		x.type := B.boolType; y := expression0(); CheckInt(y);
		par := NewNode(S.par, y, NIL); x.right := par;
		Check0(S.comma); y := expression0(); CheckInt(y);
		par2 := NewNode(S.par, y, NIL); par.right := par2
	ELSIF f.id = 'VAL THEN y := qualident();
		IF y.class # B.cType THEN Mark('not type')
		ELSIF y.type.form IN {B.tArray, B.tRec} THEN Mark('not scalar')
		END;
		par := NewNode(S.par, y, NIL); x.right := par; x.type := y.type;
		Check0(S.comma); y := expression0();
		IF y.type.form IN {B.tArray, B.tRec} THEN Mark('not scalar') END;
		par2 := NewNode(S.par, y, NIL); par.right := par2
	END
END StdFunc;

PROCEDURE element(): B.Object;
	VAR x, y: B.Object;
BEGIN
	x := expression0(); CheckInt(x);
	IF sym = S.upto THEN
		y := expression0(); CheckInt(y);
		x := NewNode(S.upto, x, y)
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
				u := NewNode(S.comma, element(), NIL);
				t.right := u; t := u
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
	IF sym = S.int THEN x := B.NewConst(B.intType, S.ival); GetSym
	ELSIF sym = S.real THEN x := B.NewConst(B.realType, S.ival); GetSym
	ELSIF sym = S.string THEN x := B.NewStr(S.str, S.slen); GetSym
	ELSIF sym = S.nil THEN x := B.NewConst(B.nilType, 0); GetSym
	ELSIF sym = S.true THEN x := B.NewConst(B.boolType, 1); GetSym
	ELSIF sym = S.false THEN x := B.NewConst(B.boolType, 0); GetSym
	ELSIF sym = S.lbrace THEN x := set()
	ELSIF sym = S.ident THEN x := designator();
		IF x.class = B.cSProc THEN Mark('not function');
			x := B.NewConst(B.intType, 0)
		ELSIF x.class = B.cSFunc THEN
			IF sym # S.lparen THEN Mark('invalid factor');
				x := B.NewConst(B.intType, 0)
			ELSE x := StdFunc(x(B.SProc))
			END
		ELSIF (sym = S.lparen) & (x.type.form = B.tProc) THEN
			IF x.type.base = NIL THEN Mark('not function') END;
			x := Call(x); IF x.type = NIL THEN x.type := B.intType END
		END
	ELSIF sym = S.lparen THEN GetSym; x := expression0(); Check0(S.rparen)
	ELSIF sym = S.not THEN GetSym; x := factor(); CheckBool(x);
		x := NewNode(S.not, x, NIL); x.type := B.boolType
	ELSE Mark('Invalid factor'); x := B.NewConst(B.intType, 0)
	END;
	RETURN x
END factor;

PROCEDURE term(): B.Object;
	VAR x, y: B.Object; xtype: B.Type; op: INTEGER;
BEGIN x := factor();
	WHILE sym = S.times DO
		Check1(x, {B.tInt, B.tReal, B.tSet}); GetSym; y := factor();
		IF ~CompTypes(xtype, y.type) THEN Mark('invalid type') END;
		xtype := x.type; x := NewNode(S.times, x, y); x.type := xtype
	ELSIF sym = S.rdiv DO
		Check1(x, {B.tReal, B.tSet}); GetSym; y := factor();
		IF ~CompTypes(xtype, y.type) THEN Mark('invalid type') END;
		xtype := x.type; x := NewNode(S.times, x, y); x.type := xtype
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
	VAR x, y: B.Object; xtype: B.Type; op: INTEGER;
BEGIN
	IF sym = S.plus THEN GetSym; x := term()
	ELSIF sym = S.minus THEN
		GetSym; x := term(); xtype := x.type;
		Check1(x, {B.tInt, B.tReal, B.tSet});
		x := NewNode(S.minus, x, NIL); x.type := xtype
	ELSE x := term()
	END;
	WHILE (sym = S.plus) OR (sym = S.minus) DO
		Check1(x, {B.tInt, B.tReal, B.tSet}); op := sym; GetSym; y := term();
		IF ~CompTypes(xtype, y.type) THEN Mark('invalid type') END;
		xtype := x.type; x := NewNode(op, x, y); x.type := xtype
	ELSIF sym = S.or DO
		CheckBool(x); GetSym; y := term(); CheckBool(y);
		x := NewNode(S.or, x, y); x.type := B.boolType
	END;
	RETURN x
END SimpleExpression;

PROCEDURE expression(): B.Object;
	VAR x, y: B.Object; tp: B.Type; op: INTEGER;
BEGIN x := SimpleExpression();
	IF (sym >= S.eql) & (sym <= S.geq) THEN
		CheckLeft(x, sym); op := sym; GetSym; y := SimpleExpression();
		IF ~CompTypes2(x.type, y.type) THEN Mark('invalid type') END;
		x := NewNode(op, x, y); x.type := B.boolType
	ELSIF sym = S.in THEN
		CheckInt(x); GetSym; y := SimpleExpression(); CheckSet(y);
		x := NewNode(S.in, x, y); x.type := B.boolType
	ELSIF sym = S.is THEN
		CheckLeft(x, S.is); GetSym;
		IF sym = S.ident THEN y := qualident() ELSE Missing(S.ident) END;
		IF (y # NIL) & (y.class = B.cType) THEN tp := y.type;
			IF (tp.form = B.tPtr) & (tp.base # NIL) & IsExt(tp, x.type)
			OR (tp.form = B.tRec) & IsExt(tp, x.type)
			THEN (*valid*) ELSE Mark('invalid type')
			END
		ELSE Mark('not type')
		END;
		x := NewNode(S.is, x, y); x.type := B.boolType
	END;
	RETURN x
END expression;

PROCEDURE ConstExpression(): B.Object;
	VAR x: B.Object;
BEGIN x := expression();
	IF IsConst(x) THEN (*valid*)
	ELSE Mark('not const'); x := B.NewConst(B.intType, 0)
	END;
	RETURN x
END ConstExpression;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE If(lev: INTEGER): B.Node;
	VAR x: B.Object; if, then: B.Node;
BEGIN
	GetSym; x := expression(); CheckBool(x); Check0(S.then);
	then := NewNode(S.then, StatementSequence0(), NIL);
	if := NewNode(S.if, x, then);
	IF sym = S.elsif THEN then.right := If(lev+1)
	ELSIF sym = S.else THEN GetSym; then.right := StatementSequence0()
	END;
	IF lev = 0 THEN Check0(S.end) END;
	RETURN if
END If;

PROCEDURE While(lev: INTEGER): B.Node;
	VAR x: B.Object; while, do: B.Node;
BEGIN
	GetSym; x := expression(); CheckBool(x); Check0(S.do);
	do := NewNode(S.do, StatementSequence0(), NIL);
	while := NewNode(S.while, x, do);
	IF sym = S.elsif THEN do.right := While(lev+1) END;
	IF lev = 0 THEN Check0(S.end) END;
	RETURN while
END While;

PROCEDURE For(): B.Node;
	VAR x: B.Object; for, control, beg, end: B.Node;
BEGIN
	for := NewNode(S.for, NIL, NIL); GetSym;
	IF sym = S.ident THEN x := FindIdent() ELSE Missing(S.ident) END;
	IF (x # NIL) & (x IS B.Var) THEN CheckInt(x);
		IF x(B.Var).ronly THEN Mark('read only') END
	ELSE Mark('not var')
	END;
	control := NewNode(S.null, x, NIL); for.left := control;
	Check0(S.becomes); x := expression(); CheckInt(x);
	beg := NewNode(S.null, x, NIL); control.right := beg;
	Check0(S.to); x := expression(); CheckInt(x);
	end := NewNode(S.null, x, NIL); beg.right := end;
	IF sym = S.by THEN
		GetSym; x := ConstExpression(); CheckInt(x);
		end.right := NewNode(S.by, x, NIL)
	END;
	Check0(S.do); for.right := StatementSequence0(); Check0(S.end);
	RETURN for
END For;

PROCEDURE Case(): B.Node;
	VAR x, y: B.Object; case, prevbar, bar: B.Node;
		
	PROCEDURE TypeCase(x: B.Object): B.Node;
		VAR bar: B.Node; y: B.Object; xtype: B.Type;
			xform, yform: INTEGER;
	BEGIN
		bar := NewNode(S.bar, NIL, NIL); y := qualident();
		xtype := x.type; xform := x.type.form; yform := y.type.form;
		IF (y # NIL) & (y.class = B.cType) THEN
			IF (xform = B.tPtr) & (yform = B.tPtr) & IsExt(y.type, xtype)
			OR (xform = B.tRec) & (yform = B.tRec) & IsExt(y.type, xtype)
			THEN x.type := y.type
			ELSIF (xform = B.tRec) & (yform = B.tPtr) & IsExt(y.type, xtype)
			THEN x.type := y.type.base
			ELSE Mark('invalid type')
			END
		ELSE Mark('not type')
		END;
		Check0(S.colon); bar.left := NewNode(S.null, y, StatementSequence0());
		x.type := xtype;
		RETURN bar
	END TypeCase;
		
BEGIN (* Case *)
	case := NewNode(S.case, NIL, NIL);
	GetSym; x := expression(); case.left := x;
	IF TypeTestable(x) & (x IS B.Var) THEN (*valid*)
	ELSE Mark('invalid case expression')
	END;
	Check0(S.of);
	REPEAT
		IF sym = S.bar THEN GetSym END;
		IF sym # S.bar THEN bar := TypeCase(x);
			IF case.right = NIL THEN case.right := bar
			ELSE prevbar.right := bar
			END;
			prevbar := bar
		END
	UNTIL sym # S.bar;
	Check0(S.end);
	RETURN case
END Case;

PROCEDURE StatementSequence(): B.Node;
	VAR x, y: B.Object; statseq, stat, nextstat, repeat: B.Node;
BEGIN
	statseq := NewNode(S.semicolon, NIL, NIL); stat := statseq;
	REPEAT (*sync*)
		IF (sym = S.ident) OR (sym >= S.semicolon)
		OR (sym >= S.if) & (sym <= S.for) THEN (*valid*)
		ELSE
			Mark('Statement?');
			REPEAT GetSym UNTIL (sym = S.ident) OR (sym >= S.semicolon)
		END;
		IF sym = S.ident THEN x := designator();
			IF sym = S.becomes THEN
				IF ~(x IS B.Var) THEN Mark('not var')
				ELSIF x(B.Var).ronly THEN Mark('read only')
				END;
				GetSym; y := expression();
				IF CompTypes(x.type, y.type)
				OR (x.type.form = B.tArray) & IsOpenArray(y.type)
					& (y.type.base = x.type.base)
				THEN (*valid*) ELSE Mark('Invalid assignment')
				END;
				stat.left := NewNode(S.becomes, x, y)
			ELSIF sym = S.eql THEN
				Mark('Should be :='); GetSym; y := expression()
			ELSIF x.type.form = B.tProc THEN
				IF x.type.base # NIL THEN Mark('Not proper procedure') END;
				stat.left := Call(x)
			ELSE Mark('Invalid statement')
			END
		ELSIF sym = S.if THEN stat.left := If(0)
		ELSIF sym = S.while THEN stat.left := While(0)
		ELSIF sym = S.repeat THEN
			GetSym; repeat := NewNode(S.repeat, StatementSequence0(), NIL);
			Check0(S.until); x := expression(); CheckBool(x);
			repeat.right := x; stat.left := repeat
		ELSIF sym = S.for THEN stat.left := For()
		ELSIF sym = S.case THEN stat.left := Case()
		END;
		IF sym <= S.semicolon THEN Check0(S.semicolon);
			nextstat := NewNode(S.semicolon, NIL, NIL);
			stat.right := nextstat; stat := nextstat
		END
	UNTIL sym > S.semicolon;
	RETURN statseq
END StatementSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FormalType(): B.Type;
	VAR x: B.Object; tp: B.Type;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF (x # NIL) & (x.class = B.cType) THEN tp := x.type
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
	VAR ronly: BOOLEAN; cls: INTEGER;
		first, ident: B.Ident; tp: B.Type;
BEGIN
	IF sym = S.var THEN cls := B.cRef; GetSym ELSE cls := B.cVar END;
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
	Check0(S.colon); tp := FormalType(); ident := first;
	ronly := (cls = B.cVar) & (tp.form IN {B.tArray, B.tRec});
	WHILE ident # NIL DO
		ident.obj := B.NewPar(proc, tp, cls, ronly); ident := ident.next
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
		IF sym = S.ident THEN x := qualident() ELSE Missing(S.ident) END;
		IF (x # NIL) & (x.class = B.cType) THEN
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
			IF (x.class = B.cType) & (x.type.form = B.tRec) THEN
				ptrType.base := x.type
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
	END
END FieldList;

PROCEDURE BaseType(): B.Type;
	VAR btype, p: B.Type; x: B.Object;
BEGIN
	IF sym = S.ident THEN x := qualident();
		IF x # NIL THEN
			IF (x.class = B.cType) & (x.type.form = B.tRec) THEN
				btype := x.type
			ELSIF (x.class = B.cType) & (x.type.form = B.tPtr) THEN
				p := x.type;
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
	VAR x: B.Object; n: INTEGER;
BEGIN x := ConstExpression(); n := 0;
	IF x.type.form = B.tInt THEN n := x(B.Const).val ELSE Mark('not int') END;
	IF n < 0 THEN Mark('invalid array length') END;
	RETURN n
END length;

PROCEDURE type(): B.Type;
	VAR tp, lastArr, t: B.Type; x: B.Object; proc: B.Proc;
		ident: B.Ident; len: INTEGER;
BEGIN tp := B.intType;
	IF sym = S.ident THEN x := qualident();
		IF (x # NIL) & (x.class = B.cType) THEN tp := x.type
		ELSE Mark('not type')
		END
	ELSIF sym = S.array THEN
		GetSym; len := length(); tp := B.NewArray(len); lastArr := tp;
		WHILE sym = S.comma DO GetSym;
			IF sym <= S.ident THEN len := length();
				lastArr.base := B.NewArray(len); lastArr := lastArr.base
			ELSE Mark('remove ,')
			END
		END;
		Check0(S.of); lastArr.base := type()
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

PROCEDURE DeclarationSequence;
	VAR first, ident, par: B.Ident; x: B.Object; tp: B.Type;
		proc: B.Proc; varobj: B.Var; statseq: B.Node;
		undef, prev: UndefPtrList;
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
			ident := NewIdent(S.id);
			NEW(x); x.class := B.cType; GetSym; Check0(S.eql);
			IF sym # S.pointer THEN x.type := type()
			ELSE
				IF ident # NIL THEN ident.obj := x END;
				x.type := PointerType(x)
			END;
			Check0(S.semicolon);
			IF (B.curLev = 0) & (ident # NIL) & (x.type.form = B.tRec) THEN
				undef := undefList; prev := NIL;
				WHILE (undef # NIL) & (undef.name # ident.name) DO
					prev := undef; undef := undef.next
				END;
				IF undef # NIL THEN undef.tp.base := x.type;
					IF prev # NIL THEN prev.next := undef.next
					ELSE undefList := undef.next
					END
				END
			END
		END;
		IF undefList # NIL THEN Mark('some pointers didnt have base type') END
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
				ident.obj := B.NewVar(tp); ident := ident.next
			END;
		END
	END;
	WHILE sym = S.procedure DO GetSym;
		IF sym = S.ident THEN curProcIdent := NewIdent(S.id); GetSym
		ELSE curProcIdent := NIL; Mark('proc name?')
		END;
		proc := B.NewProc(); tp := B.NewProcType(); proc.type := tp;
		IF sym = S.lparen THEN FormalParameters(tp) END; Check0(S.semicolon);
		B.OpenScope; B.IncLev(1); par := tp.fields;
		WHILE par # NIL DO
			ident := NewIdent(par.name); NEW(varobj); ident.obj := varobj;
			varobj^ := par.obj(B.Var)^; par := par.next
		END;
		ident := curProcIdent; DeclarationSequence();
		curProcIdent := ident; proc.decl := B.topScope.first;
		IF curProcIdent # NIL THEN curProcIdent.obj := proc END;		
		IF sym = S.begin THEN GetSym; proc.statseq := StatementSequence() END;
		IF sym = S.return THEN
			IF tp.base = NIL THEN Mark('not function proc') END;
			x := expression(); proc.return := x;
			IF x.type.form IN {B.tArray, B.tRec} THEN Mark('invalid type') END                
		ELSIF tp.base # NIL THEN Missing(S.return)
		END;	
		B.CloseScope; B.IncLev(-1); Check0(S.end);
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
	VAR modid: B.IdStr; modinit: B.Node;
BEGIN GetSym; modid[0] := 0X;
	IF sym = S.ident THEN modid := S.id; GetSym ELSE Missing(S.ident) END;
	Check0(S.semicolon); DeclarationSequence;
	IF sym = S.begin THEN GetSym; modinit := StatementSequence() END;
	Check0(S.end);
	IF sym = S.ident THEN
		IF S.id # modid THEN Mark('wrong module name') END; GetSym
	ELSE Missing(S.ident)
	END;
	Check0(S.period);
	IF S.errcnt = 0 THEN G.Generate(modid, modinit) END
END Module;
	
BEGIN
	type0 := type; expression0 := expression;
	StatementSequence0 := StatementSequence
END Parser1.
