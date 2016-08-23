MODULE Parser0;

IMPORT
	Base := Base0,
	Scanner := Scanner0,
	SymTable := SymTable0,
	Generator := Generator0;

CONST
	noSemicolonError = 'No ;';
	noColonError = 'No :';
	noEqlError = 'No =';
	noEndError = 'No END';
	noOfError = 'No OF';
	noToError = 'No TO';
	noIdentError = 'Identifier expected';
	noRParenError = 'No )';
	noRBrakError = 'No ]';
	
	notConstError = 'Not a const';
	notTypeError = 'Not a type';
	notFieldListError = 'Not a field list';
	notRecordTypeError = 'Not a record type';
	notCompTypeError = 'Not compatible type';
	
	superfluousCommaError = 'Superfluous ,';
	superfluousSemicolonError = 'Superfluous ;';
	superfluousBarError = 'Superfluous |';
	
TYPE
	UndefPtrList = POINTER TO RECORD
		name: Base.IdStr; tp: Base.Type; next: UndefPtrList
	END;
	
VAR
	sym*: INTEGER;
	undefList: UndefPtrList;
	defObj: Base.Object;
	noError: BOOLEAN;
	
	type0: PROCEDURE(VAR tp: Base.Type);
	expression0: PROCEDURE(VAR x: Base.Item);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Error(err: ARRAY OF CHAR);
BEGIN Scanner.Mark(err); noError := FALSE
END Error;

PROCEDURE NextSym;
BEGIN Scanner.Get(sym); noError := TRUE
END NextSym;

PROCEDURE Check(expected: INTEGER; err: ARRAY OF CHAR);
BEGIN IF sym = expected THEN NextSym ELSE Error(err) END
END Check;

PROCEDURE MakeIntConst(VAR x: Base.Item);
END MakeIntConst;

PROCEDURE CheckInt(VAR x: Base.Item);
BEGIN
	IF x.type.form # Base.tInt THEN
		Error('Not an integer'); x.type := Base.intType
	END
END CheckInt;

PROCEDURE CheckBool(VAR x: Base.Item);
BEGIN
	IF x.type.form # Base.tBool THEN
		Error('Not a bool'); x.type := Base.boolType
	END
END CheckBool;

PROCEDURE CheckArrayLen(VAR x: Base.Item);
BEGIN CheckInt(x);
	IF x.mode # Base.cConst THEN Error(notConstError); x.a := 1
	ELSIF x.a < 1 THEN Error('Invalid array length'); x.a := 1
	END
END CheckArrayLen;

PROCEDURE IsOpenArray(tp: Base.Type): BOOLEAN;
	RETURN (tp.form = Base.tArray) & (tp.len = 0)
END IsOpenArray;

PROCEDURE IsString(VAR x: Base.Item): BOOLEAN;
	RETURN (x.type.form = Base.tArray) & (x.type.base.form = Base.tChar)
	OR (x.type.form = Base.tStr)
	OR (x.mode = Base.cConst) & (x.type.form = Base.tChar)
END IsString;

PROCEDURE IsChar(VAR x: Base.Item): BOOLEAN;
	RETURN (x.type.form = Base.tChar)
	OR (x.type.form = Base.tStr) & (x.type.len <= 2)
END IsChar;

PROCEDURE IsVarPar(VAR x: Base.Item): BOOLEAN;
	RETURN (x.mode = Base.cRef) & ~x.readOnly
END IsVarPar;

PROCEDURE TypeTestable(VAR x: Base.Item): BOOLEAN;
	RETURN (x.type.form = Base.tPtr)
	OR IsVarPar(x) & (x.type.form = Base.tRec)
END TypeTestable;

PROCEDURE IsExt0(tp1, tp2: Base.Type): BOOLEAN;
	RETURN (tp1 = tp2)
	OR (tp1.len > tp2.len) & IsExt0(tp1.base, tp2)
END IsExt0;

PROCEDURE IsExt(tp1, tp2: Base.Type): BOOLEAN;
BEGIN
	IF tp1.form = Base.tPtr THEN tp1 := tp1.base END;
	IF tp2.form = Base.tPtr THEN tp2 := tp2.base END;
	RETURN IsExt0(tp1, tp2)
END IsExt;

(* Assignment check *)
PROCEDURE CompType(t1, t2: Base.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = Base.tInt) & (t2.form = Base.tInt)
	OR (t1.form = Base.tArray) & (t2.form = Base.tArray)
		& (t1.base = t2.base) & (t1.len = t2.len)
	OR (t1.form = Base.tPtr) & (t2.form = Base.tPtr) & IsExt(t2, t1)
	OR (t1.form IN {Base.tPtr, Base.tProc}) & (t2 = Base.nilType)
END CompType;

(* Comparison check *)
PROCEDURE CompType2(t1, t2: Base.Type): BOOLEAN;
	RETURN CompType(t1, t2) OR CompType(t2, t1)
END CompType2;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident(VAR obj: Base.Object);
BEGIN
	SymTable.Find(obj, Scanner.id);
	IF obj = NIL THEN Error('Undefined identifier') END
END qualident;

PROCEDURE ActualParameters(VAR x: Base.Item);
END ActualParameters;

PROCEDURE set(VAR x: Base.Item);
END set;

PROCEDURE TypeTest(VAR x: Base.Item; guard: BOOLEAN);
	VAR obj: Base.Object;
		tp: Base.Type;
BEGIN
	IF sym = Scanner.ident THEN qualident(obj) ELSE Error('No type?') END;
	IF noError THEN
		IF (obj.class = Base.cType) & (obj.type.form = x.type.form) THEN
			tp := obj.type;
			IF IsExt(x.type, tp) OR IsExt(tp, x.type) THEN
				Generator.TypeTest(x, tp, guard)
			ELSE Error(notCompTypeError)
			END
		ELSIF obj.class = Base.cType THEN Error(notCompTypeError)
		ELSE Error(notTypeError)
		END;
		IF ~noError & ~guard THEN Generator.MakeConst(x, Base.boolType, 0) END
	END
END TypeTest;

PROCEDURE designator(VAR x: Base.Item);
	VAR obj, fld: Base.Object;
		idx: Base.Item;
		id: Base.IdStr;
		xform: INTEGER;
		valid: BOOLEAN;
BEGIN qualident(obj);
	IF (obj # NIL) & (obj.class IN Base.clsValue) THEN
		Generator.MakeItem(x, obj); xform := x.type.form; valid := TRUE
	ELSE valid := FALSE; Error('Invalid designator');
		Generator.MakeConst(x, Base.intType, 0)
	END;
	Generator.MakeItem(x, obj); xform := x.type.form;
	WHILE sym = Scanner.period DO
		IF valid & (xform # Base.tPtr) & (xform # Base.tRec) THEN
			Error('Not a pointer or record'); valid := FALSE
		END;
		NextSym;
		IF valid & (sym # Scanner.ident) THEN
			Error('No record field?'); valid := FALSE
		END;
		IF valid THEN id := Scanner.id;
			IF xform = Base.tPtr THEN Generator.Deref(x) END;
			IF xform = Base.tRec THEN fld := x.type.fields;
				WHILE (fld # NIL) & (fld.name # id) DO fld := fld.next END;
				IF fld # NIL THEN Generator.Field(x, fld)
				ELSE Error('Field not found'); valid := FALSE
				END
			END;
			NextSym
		END
	ELSIF sym = Scanner.lbrak DO
		IF valid & (xform # Base.tArray) THEN
			Error('Not an array'); valid := FALSE
		END;
		NextSym; expression0(idx); CheckInt(idx);
		IF valid & (xform = Base.tArray) THEN Generator.Index(x, idx) END;
		WHILE sym = Scanner.comma DO
			NextSym; expression0(idx); CheckInt(idx);
			IF valid & (x.type.form # Base.tArray) THEN
				Error('Not a multi-dimensional array'); valid := FALSE
			END;
			IF valid & (x.type.form = Base.tArray) THEN
				Generator.Index(x, idx)
			END
		END;
		Check(Scanner.rbrak, noRBrakError)
	ELSIF sym = Scanner.arrow DO
		IF valid & (xform # Base.tPtr) THEN
			Error('Not a pointer'); valid := FALSE
		END;
		IF valid THEN Generator.Deref(x) END; NextSym
	ELSIF (sym = Scanner.lparen) & TypeTestable(x) DO
		IF valid THEN NextSym; TypeTest(x, TRUE);
		ELSE NextSym; IF sym = Scanner.ident THEN qualident(obj) END 
		END;
		Check(Scanner.rparen, noRParenError)
	END
END designator;

PROCEDURE factor(VAR x: Base.Item);
BEGIN
	IF sym = Scanner.int THEN
		Generator.MakeConst(x, Base.intType, Scanner.ival)
	ELSIF sym = Scanner.real THEN
		Generator.MakeConst(x, Base.realType, Scanner.ival)
	ELSIF sym = Scanner.char THEN
		Generator.MakeConst(x, Base.charType, Scanner.ival)
	ELSIF sym = Scanner.string THEN
		Generator.MakeStr(x, Scanner.str, Scanner.slen, Scanner.ansiStr)
	ELSIF sym = Scanner.nil THEN Generator.MakeConst(x, Base.nilType, 0)
	ELSIF sym = Scanner.true THEN Generator.MakeConst(x, Base.boolType, 1)
	ELSIF sym = Scanner.false THEN Generator.MakeConst(x, Base.boolType, 0)
	ELSIF sym = Scanner.lbrace THEN set(x)
	ELSIF sym = Scanner.ident THEN designator(x);
		IF sym = Scanner.lparen THEN
			IF (x.type.form = Base.tProc) & (x.type.base # NIL) THEN
				ActualParameters(x); Generator.Call(x);
				Generator.ReturnValue(x)
			ELSE Error('Not a function procedure')
			END
		END
	ELSIF sym = Scanner.lparen THEN
		NextSym; expression0(x); Check(Scanner.rparen, noRParenError)
	ELSIF sym = Scanner.not THEN
		NextSym; factor(x); CheckBool(x); Generator.Not(x)
	ELSE Error('Invalid factor')
	END
END factor;

PROCEDURE term(VAR x: Base.Item);
	VAR y: Base.Item;
		op, xform, yform: INTEGER;
		errorFlag: BOOLEAN;
BEGIN
	errorFlag := FALSE; factor(x); xform := x.type.form;
	IF (sym >= Scanner.times) & (sym <= Scanner.mod) THEN
		Generator.LoadVolatile(x); op := sym;
		NextSym; factor(y); yform := y.type.form;
		IF (xform = Base.tInt) & (yform = Base.tInt) THEN
			IF op = Scanner.times THEN Generator.IntMul(x, y)
			ELSIF op = Scanner.div THEN Generator.IntDiv(x, y, TRUE)
			ELSIF op = Scanner.mod THEN Generator.IntDiv(x, y, FALSE)
			ELSE errorFlag := TRUE
			END
		ELSIF (xform = Base.tReal) & (y.type = x.type) & (op <= Scanner.rdiv)
		THEN Generator.RealOp(x, y, op)
		ELSIF (xform = Base.tSet) & (yform = Base.tSet) & (op <= Scanner.rdiv)
		THEN Generator.SetOp(x, y, op)
		ELSE errorFlag := TRUE
		END
	ELSIF sym = Scanner.and THEN
		IF xform = Base.tBool THEN Generator.And1(x) END;
		NextSym; term(y); yform := y.type.form;
		IF (xform = Base.tBool) & (yform = Base.tBool) THEN
			Generator.And2(x, y)
		ELSE errorFlag := TRUE
		END
	END;
	IF errorFlag THEN
		Error('Invalid term'); MakeIntConst(y); MakeIntConst(x)
	END
END term;

PROCEDURE SimpleExpression(VAR x: Base.Item);
	VAR y: Base.Item;
		op, xform, yform: INTEGER;
		errorFlag: BOOLEAN;
BEGIN errorFlag := FALSE;
	IF sym = Scanner.plus THEN NextSym; term(x)
	ELSIF sym = Scanner.minus THEN NextSym; term(x);
		IF x.type.form IN {Base.tInt, Base.tSet, Base.tReal} THEN
			Generator.Negate(x)
		ELSE Error('Minus sign?')
		END
	ELSE term(x)
	END;
	xform := x.type.form;
	IF (sym >= Scanner.plus) & (sym <= Scanner.minus) THEN
		Generator.LoadVolatile(x); op := sym;
		NextSym; term(y); yform := y.type.form;
		IF (xform = Base.tInt) & (yform = Base.tInt) THEN
			Generator.IntAdd(x, y, op)
		ELSIF (xform = Base.tReal) & (x.type = y.type) THEN
			Generator.RealOp(x, y, op)
		ELSIF (xform = Base.tSet) & (yform = Base.tSet) THEN
			Generator.SetOp(x, y, op)
		ELSE errorFlag := TRUE
		END
	ELSIF sym = Scanner.or THEN
		IF xform = Base.tBool THEN Generator.Or1(x) END;
		NextSym; term(y); yform := y.type.form;
		IF (xform = Base.tBool) & (yform = Base.tBool) THEN
			Generator.Or2(x, y)
		ELSE errorFlag := TRUE
		END
	END;
	IF errorFlag THEN
		Error('Invalid simple expression');
		MakeIntConst(y); MakeIntConst(x)
	END
END SimpleExpression;

PROCEDURE expression(VAR x: Base.Item);
	VAR y: Base.Item;
		obj: Base.Object;
		xform, yform, rel: INTEGER;
		errorFlag: BOOLEAN;
BEGIN
	SimpleExpression(x); xform := x.type.form; errorFlag := FALSE;
	IF (sym = Scanner.eql) OR (sym <= Scanner.geq) THEN
		rel := sym; NextSym; SimpleExpression(y); yform := y.type.form;
		IF (xform = Base.tInt) & (yform = Base.tInt)
		OR IsChar(x) & IsChar(y) THEN
			Generator.Compare(x, y, rel)
		ELSIF (xform = Base.tReal) & (x.type = y.type) THEN
			Generator.RealCompare(x, y, rel)
		ELSIF IsString(x) & IsString(y) THEN
			Generator.StrCompare(x, y, rel)
		ELSE errorFlag := TRUE
		END;
		IF errorFlag & (sym <= Scanner.neq) THEN
			IF (xform IN Base.typEql) & CompType2(x.type, y.type) THEN
				Generator.Compare(x, y, rel); errorFlag := FALSE
			END
		END
	ELSIF sym = Scanner.in THEN
		NextSym; SimpleExpression(y); yform := y.type.form;
		IF (xform = Base.tInt) & (yform = Base.tSet) THEN
			Generator.Membership(x, y)
		ELSE errorFlag := TRUE
		END
	ELSIF sym = Scanner.is THEN NextSym;
		IF TypeTestable(x) THEN TypeTest(x, FALSE)
		ELSE errorFlag := TRUE; IF sym = Scanner.ident THEN qualident(obj) END
		END
	END;
	IF errorFlag THEN
		Error('Invalid expression'); MakeIntConst(y);
		MakeIntConst(x); x.type := Base.boolType
	END
END expression;

PROCEDURE StatementSequence;
BEGIN
END StatementSequence;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FieldList(tp: Base.Type);
	VAR first, field: Base.Object;
		fieldType: Base.Type;
		n: INTEGER;
BEGIN SymTable.New(first, Scanner.id, Base.cField); NextSym;
	WHILE sym = Scanner.comma DO NextSym;
		IF sym = Scanner.ident THEN
			SymTable.New(field, Scanner.id, Base.cField);
			IF first = NIL THEN first := field END; NextSym
		ELSE Error(superfluousCommaError)
		END
	END;
	Check(Scanner.colon, noColonError); type0(fieldType);
	n := tp.size; n := n + (-n) MOD fieldType.align; tp.size := n;
	IF fieldType.align > tp.align THEN tp.align := fieldType.align END;
	field := first;
	WHILE field # NIL DO
		field.type := fieldType; field.lev := SymTable.curLev;
		field.val := n; n := n + fieldType.size; tp.size := n;
		field := field.next
	END;
END FieldList;

PROCEDURE FormalType(VAR tp: Base.Type);
	VAR obj: Base.Object;
BEGIN tp := Base.intType;
	IF sym = Scanner.ident THEN qualident(obj);
		IF obj.class = Base.cType THEN tp := obj.type
		ELSE Error(notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		Base.NewType(tp, Base.tArray); tp.len := 0;
		NextSym; Check(Scanner.of, noOfError); FormalType(tp.base);
		IF (tp.base.form # Base.tArray) OR (tp.base.len # 0) THEN
			tp.size := Base.WordSize * 2
		ELSE tp.size := tp.base.size + Base.WordSize
		END
	END
END FormalType;

PROCEDURE FPSection(proc: Base.Type);
	VAR cl, szInStack: INTEGER;
		first, obj: Base.Object;
		tp: Base.Type;
		ronly: BOOLEAN;
BEGIN
	IF sym = Scanner.var THEN cl := Base.cRef; NextSym ELSE cl := Base.cVar END;
	IF sym = Scanner.ident THEN
		SymTable.New(first, Scanner.id, cl);
		WHILE sym = Scanner.comma DO NextSym;
			IF sym = Scanner.ident THEN
				SymTable.New(obj, Scanner.id, cl); NextSym;
				IF first = NIL THEN first := obj END
			ELSE Error(superfluousCommaError)
			END
		END
	ELSE Error('No params?')
	END;
	Check(Scanner.colon, noColonError); FormalType(tp);
	ronly := FALSE; szInStack := Base.WordSize;
	IF (tp.form = Base.tArray) & (tp.len = 0) THEN szInStack := tp.size END;
	IF (cl = Base.cVar) & (tp.form IN {Base.tArray, Base.tRec}) THEN
		IF ~IsOpenArray(tp) & ~Generator.FitInReg(tp) THEN cl := Base.cRef END;
		ronly := TRUE
	END;
	obj := first;
	WHILE obj # NIL DO
		obj.class := cl; obj.readOnly := ronly; obj.type := tp;
		obj.val := proc.parblksize; obj := obj.next; INC(proc.nopar);
		proc.parblksize := proc.parblksize + szInStack
	END
END FPSection;

PROCEDURE FormalParameters(proc: Base.Type);
	VAR obj: Base.Object;
BEGIN NextSym;
	IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
		SymTable.OpenScope(''); FPSection(proc);
		WHILE sym = Scanner.semicolon DO NextSym;
			IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN FPSection(proc)
			ELSE Error(superfluousSemicolonError)
			END
		END;
		proc.fields := SymTable.topScope.next; SymTable.CloseScope
	END;
	Check(Scanner.rparen, noRParenError);
	IF sym = Scanner.colon THEN NextSym; qualident(obj);
		IF obj.class = Base.cType THEN proc.base := obj.type;
			IF obj.type.form IN {Base.tArray, Base.tRec} THEN
				Error('Invalid return type'); proc.base := Base.intType
			END
		ELSE Error(notTypeError)
		END
	END
END FormalParameters;

PROCEDURE NewProcType(VAR tp: Base.Type);
BEGIN
	Base.NewType(tp, Base.tProc); tp.size := Base.WordSize;
	tp.align := Base.WordSize; tp.parblksize := 0; tp.nopar := 0;
	IF sym = Scanner.lparen THEN FormalParameters(tp) END
END NewProcType;

PROCEDURE type(VAR tp: Base.Type);
	VAR obj: Base.Object;
		x: Base.Item;
		tpArray: Base.Type;
		undef: UndefPtrList;
		id: Base.IdStr;
		
	PROCEDURE CalculateArraySize(tp, tp2: Base.Type);
	BEGIN
		IF tp # tp2 THEN CalculateArraySize(tp.base, tp2) END;
		tp.size := tp.base.size * tp.len; tp.align := tp.base.align
	END CalculateArraySize;
	
BEGIN (* type *)
	tp := Base.intType;
	IF sym = Scanner.ident THEN qualident(obj);
		IF obj.class = Base.cType THEN
			IF (obj # defObj) OR (obj.type.form = Base.tPtr) THEN
				tp := obj.type
			ELSE Error('Circular definition')
			END
		ELSE Error(notTypeError)
		END
	ELSIF sym = Scanner.array THEN
		NextSym; expression(x); CheckArrayLen(x);
		Base.NewType(tpArray, Base.tArray); tpArray.len := x.a; tp := tpArray;
		WHILE sym = Scanner.comma DO
			NextSym; expression(x); CheckArrayLen(x);
			Base.NewType(tpArray.base, Base.tArray);
			tpArray := tpArray.base; tpArray.len := x.a
		END;
		Check(Scanner.of, noOfError); type(tpArray.base);
		CalculateArraySize(tp, tpArray)
	ELSIF sym = Scanner.record THEN
		Base.NewType(tp, Base.tRec); tp.size := 0; tp.align := 0;
		tp.len := 0; NextSym;
		IF sym = Scanner.lparen THEN NextSym; qualident(obj);
			IF noError & (obj.class # Base.cType) THEN Error(notTypeError) END;
			IF noError & (obj.type.form # Base.tRec) THEN
				Error(notRecordTypeError)
			END;
			IF noError & (obj.lev > 0) THEN Error('Must be global type') END;
			IF noError & (obj.type.len >= Base.MaxExt-1) THEN
				Error('Too many extension levels')
			END;
			IF noError THEN tp.base := obj.type; tp.align := obj.type.align;
				tp.size := obj.type.size; tp.len := obj.type.len + 1
			END;
			Check(Scanner.rparen, noRParenError)
		END;
		SymTable.OpenScope('');
		IF sym = Scanner.ident THEN FieldList(tp);
			WHILE sym = Scanner.comma DO NextSym;
				IF sym # Scanner.ident THEN Error(superfluousCommaError) END;
				IF noError THEN FieldList(tp) ELSE NextSym END				
			END
		END;
		tp.fields := SymTable.topScope.next; SymTable.CloseScope;
		Check(Scanner.end, noEndError)
	ELSIF sym = Scanner.pointer THEN
		NextSym; Check(Scanner.to, noToError); Base.NewType(tp, Base.tPtr);
		tp.base := Base.intType; tp.size := Base.WordSize;
		tp.align := Base.WordSize;
		IF sym = Scanner.ident THEN
			id := Scanner.id; obj := SymTable.universe.next;
			WHILE (obj # NIL) & (obj.name # id) DO obj := obj.next END;
			IF obj # NIL THEN
				IF obj.class # Base.cType THEN Error(notTypeError) END;
				IF noError & (obj.type.form # Base.tRec) THEN
					Error(notRecordTypeError)
				END;
				IF noError THEN tp.base := obj.type END
			ELSIF SymTable.curLev = 0 THEN NEW(undef); undef.tp := tp;
				undef.name := id; undef.next := undefList; undefList := undef
			ELSE Error('Type not found, pointer base type must be global')
			END
		ELSIF sym = Scanner.record THEN type(tp.base)
		ELSE Error('Base type?')
		END
	ELSIF sym = Scanner.procedure THEN
		NextSym; NewProcType(tp)
	END
END type;

PROCEDURE DeclarationSequence(VAR varsize: INTEGER);
	VAR id: Base.IdStr;
		x: Base.Item;
		first, obj, proc, par: Base.Object;
		typ, proctyp: Base.Type;
		n, locblksize: INTEGER;
BEGIN
	IF sym = Scanner.const THEN NextSym;
		WHILE sym = Scanner.ident DO id := Scanner.id; NextSym;
			Check(Scanner.eql, noEqlError); expression(x);
			IF x.mode # Base.cConst THEN
				Error(notConstError); MakeIntConst(x)
			END;
			SymTable.New(obj, id, x.mode);
			IF obj # NIL THEN
				obj.type := x.type; obj.val := x.a; obj.lev := x.lev
			END;
			Check(Scanner.semicolon, noSemicolonError)
		END
	END;
	
	IF sym = Scanner.type THEN NextSym;
		WHILE sym = Scanner.ident DO
			id := Scanner.id; SymTable.New(obj, id, Base.cType);
			defObj := obj; NextSym; Check(Scanner.eql, noEqlError);
			type(typ); Check(Scanner.semicolon, noSemicolonError);
			IF obj # NIL THEN obj.type := typ END
		END
	END;
	defObj := NIL;
	
	varsize := 0;
	IF sym = Scanner.var THEN NextSym;
		WHILE sym = Scanner.ident DO
			SymTable.New(first, Scanner.id, Base.cVar);
			WHILE sym = Scanner.comma DO NextSym;
				IF sym = Scanner.ident THEN
					SymTable.New(obj, Scanner.id, Base.cVar);
					IF first = NIL THEN first := obj END; NextSym
				ELSE Error(superfluousCommaError)
				END
			END;
			Check(Scanner.colon, noColonError); type(typ); obj := first;
			WHILE obj # NIL DO
				obj.type := typ; obj.lev := SymTable.curLev;
				n := varsize - typ.size; n := n - n MOD typ.align;
				obj.val := n; varsize := n; obj := obj.next
			END;
			Check(Scanner.semicolon, noSemicolonError)
		END
	END;
	
	WHILE sym = Scanner.procedure DO NextSym;
		IF sym = Scanner.ident THEN
			id := Scanner.id; SymTable.New(proc, id, Base.cProc); NextSym
		ELSE obj := NIL; id := ''; Error('Proc name?')
		END;
		NewProcType(proctyp); Check(Scanner.semicolon, noSemicolonError);
		IF obj # NIL THEN obj.type := proctyp END;
		SymTable.OpenScope(id); par := proctyp.fields;
		WHILE par # NIL DO SymTable.New(obj, par.name, Base.cVar);
			obj^ := par^; obj.next := NIL; par := par.next
		END;
		DeclarationSequence(locblksize); Generator.Enter;
		IF sym = Scanner.begin THEN NextSym; StatementSequence END;
		IF sym = Scanner.return THEN typ := proctyp.base;
			IF typ = NIL THEN Error('RETURN in proper proc') END;
			NextSym; expression(x);
			IF (typ # NIL) & ~CompType(x.type, typ) THEN
				Error(notCompTypeError)
			END
		ELSIF proctyp.base # NIL THEN
			Error('Function procedure need RETURN clause')
		END;
		Generator.Return; Check(Scanner.end, noEndError);
		IF sym = Scanner.ident THEN
			IF Scanner.id # id THEN Error('Proc name mismatched') END; NextSym
		ELSE Error('No proc name?')
		END;
		Check(Scanner.semicolon, noSemicolonError)
	END
END DeclarationSequence;

PROCEDURE Module*;
	VAR modid: Base.IdStr; varsize: INTEGER;
BEGIN
	NextSym;
	IF sym = Scanner.ident THEN modid := Scanner.id; NextSym
	ELSE modid := '@'; Error('No module name')
	END;
	Check(Scanner.semicolon, noSemicolonError);
	IF modid # '@' THEN
		SymTable.Init(modid); Generator.Init;
		(* IF sym = Scanner.import THEN ImportList END; *)
		IF Scanner.errcnt = 0 THEN
			varsize := 0; DeclarationSequence(varsize);		
			Generator.Enter;
			IF sym = Scanner.begin THEN NextSym; StatementSequence END;
			Generator.Return;		
			Check(Scanner.end, noEndError);
			IF sym = Scanner.ident THEN
				IF modid # Scanner.id THEN Error('Wrong module name') END;
				NextSym
			ELSE Error('No module identifier')
			END;
			Check(Scanner.period, 'No ending .');
			Generator.Finish
		END
	END
END Module;

BEGIN
	type0 := type;
	expression0 := expression
END Parser0.