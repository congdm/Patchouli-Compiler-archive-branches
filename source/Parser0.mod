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
	
	notConstError = 'Not a const';
	notTypeError = 'Not a type';
	notIntTypeError = 'Not an integer';
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
	
	type0: PROCEDURE(VAR tp: Base.Type);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Error(err: ARRAY OF CHAR);
BEGIN Scanner.Mark(err)
END Error;

PROCEDURE Check(expected: INTEGER; err: ARRAY OF CHAR);
BEGIN IF sym = expected THEN Scanner.Get(sym) ELSE Scanner.Mark(err) END
END Check;

PROCEDURE MakeIntConst(VAR x: Base.Item);
END MakeIntConst;

PROCEDURE NextSym;
BEGIN Scanner.Get(sym)
END NextSym;

PROCEDURE CheckInt(VAR x: Base.Item);
BEGIN
	IF x.type.form # Base.tInteger THEN
		Error(notIntTypeError); x.type := Base.intType
	END
END CheckInt;

PROCEDURE CheckArrayLen(VAR x: Base.Item);
BEGIN CheckInt(x);
	IF x.mode # Base.cConst THEN Error(notConstError); x.a := 1
	ELSIF x.a < 1 THEN Error('Invalid array length'); x.a := 1
	END
END CheckArrayLen;

PROCEDURE CompType(t1, t2: Base.Type): BOOLEAN;
	RETURN (t1 = t2)
	OR (t1.form = Base.tArray) & (t2.form = Base.tArray)
		& (t1.base = t2.base) & (t1.len = t2.len)
	OR (t1.form = Base.tNPointer) & (t2.form = Base.tNPointer)
		& (t1.base = t2.base)
	OR (t1.form IN {Base.tNPointer, Base.tProcedure}) & (t2 = Base.nilType)
	OR (t2.form IN {Base.tNPointer, Base.tProcedure}) & (t1 = Base.nilType)
END CompType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE qualident(VAR obj: Base.Object);
BEGIN
	SymTable.Find(obj, Scanner.id)
END qualident;

PROCEDURE term(VAR x: Base.Item)

PROCEDURE SimpleExpression(VAR x: Base.Item);
	VAR y: Base.Item;
		op, xform, yform: INTEGER;
		errorFlag: BOOLEAN;
BEGIN errorFlag := FALSE;
	IF sym = Scanner.plus THEN NextSym; term(x)
	ELSIF sym = Scanner.minus THEN NextSym; term(x); Generator.Negate(x)
	ELSE term(x)
	END;
	xform := x.type.form;
	IF (sym >= Scanner.plus) & (sym <= Scanner.minus) THEN
		Generator.LoadVolatile(x); op := sym;
		NextSym; term(y); yform := y.type.form;
		IF (xform = {Base.tInteger, Base.tSet}) & (xform = yform)
		OR (xform = Base.tReal) & (x.type = y.type)
		THEN Generator.AddOp(x, y, op)
		ELSE errorFlag := TRUE
		END
	ELSIF sym = Scanner.or THEN
		IF xform = Base.tBoolean THEN Generator.Or1(x) END;
		NextSym; term(y); yform := y.type.form;
		IF (xform = Base.tBoolean) & (yform = Base.tBoolean) THEN
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
		xform, yform, rel: INTEGER;
		errorFlag: BOOLEAN;
BEGIN
	SimpleExpression(x); xform := x.type.form; errorFlag := FALSE;
	IF (sym >= Scanner.eql) OR (sym <= Scanner.geq) THEN
		rel := sym; NextSym; SimpleExpression(y); yform := y.type.form;
		IF (xform IN {Base.tInteger, Base.tChar}) & (xform = yform)
		OR (xform = Base.tReal) & (x.type = y.type)
		OR (xform IN Base.typeComparable2)
			& (rel <= Scanner.neq) & CompType(x.type, y.type)
		THEN Generator.Compare(x, y, rel)
		ELSIF IsString(x) & IsString(y) THEN Generator.StrCompare(x, y, rel)
		ELSE errorFlag := TRUE
		END
	ELSIF sym = Scanner.in THEN
		NextSym; SimpleExpression(y); yform := y.type.form;
		IF (xform = Base.tInteger) & (yform = Base.tSet) THEN
			Generator.Membership(x, y)
		ELSE errorFlag := TRUE
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
BEGIN
	SymTable.New(first, Scanner.id, Base.cField); NextSym;
	WHILE sym = Scanner.comma DO NextSym;
		IF sym = Scanner.ident THEN
			SymTable.New(field, Scanner.id, Base.cField);
			IF first = NIL THEN first := field END;
			NextSym
		ELSE Error(superfluousCommaError)
		END
	END;
	Check(Scanner.colon, noColonError); type0(fieldType);
	n := tp.size; n := n + (-n) MOD fieldType.align; tp.size := n;
	IF fieldType.align > tp.align THEN
		tp.align := fieldType.align
	END;
	field := first;
	WHILE field # NIL DO
		field.type := fieldType;
		field.lev := SymTable.curLev;
		field.val := n;
		n := n + fieldType.size; tp.size := n;
		field := field.next
	END;
END FieldList;

PROCEDURE Union(tp: Base.Type);
	VAR off, tpAlign, unionSize: INTEGER;
		field, prev: Base.Object;
BEGIN
	NextSym; off := tp.size; tp.size := 0; unionSize := 0;
	tpAlign := tp.align; tp.align := 0;
	prev := SymTable.topScope;
	WHILE prev.next # NIL DO prev := prev.next END;
	
	IF sym = Scanner.bar THEN Error(superfluousBarError); NextSym END;
	REPEAT
		IF sym = Scanner.semicolon THEN
			Error(superfluousSemicolonError); NextSym
		END;
		IF sym = Scanner.ident THEN
			(* FieldListSequence *)
			REPEAT FieldList(tp);
				IF sym = Scanner.semicolon THEN NextSym;
					IF sym = Scanner.end THEN Error(superfluousSemicolonError)
					ELSIF sym # Scanner.ident THEN Error(notFieldListError)
					END
				END
			UNTIL sym # Scanner.ident;
		END;
		IF sym = Scanner.bar THEN NextSym;
			IF sym = Scanner.end THEN Error(superfluousBarError)
			ELSIF sym # Scanner.ident THEN Error(notFieldListError)
			END;
			IF tp.size > unionSize THEN unionSize := tp.size END;
			tp.size := 0
		END;
	UNTIL sym # Scanner.ident;
	
	off := off + (-off) MOD tp.align; tp.size := off + unionSize;
	IF tpAlign > tp.align THEN tp.align := tpAlign END;
	field := prev.next;
	WHILE field # NIL DO
		field.val := field.val + off;
		field := field.next
	END;
	Check(Scanner.end, noEndError)
END Union;

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

PROCEDURE FormalParameters(proc: Base.Type);
	VAR first, obj: Base.Object;
		tp: Base.Type;
		cls, parblksize, size, nopar: INTEGER;
		ronly: BOOLEAN;
BEGIN NextSym;
	IF (sym = Scanner.ident) OR (sym = Scanner.var) THEN
		SymTable.OpenScope(''); parblksize := 0; nopar := 0;
		REPEAT
			IF sym = Scanner.var THEN cls := Base.cRef; NextSym
			ELSE cls := Base.cVar
			END;
			IF sym = Scanner.ident THEN
				SymTable.New(first, Scanner.id, cls);
				WHILE sym = Scanner.comma DO NextSym;
					IF sym = Scanner.ident THEN
						SymTable.New(obj, Scanner.id, cls); NextSym;
						IF first = NIL THEN first := obj END
					ELSE Error(superfluousCommaError)
					END
				END
			ELSE Error(noIdentError)
			END;
			Check(Scanner.colon, noColonError); FormalType(tp);
			ronly := FALSE; size := Base.WordSize;
			IF (tp.form = Base.tArray) & (tp.len = 0) THEN
				size := tp.size
			END;
			IF cls = Base.cVar THEN
				IF (tp.form = Base.tArray) & (tp.len = 0)
				OR (tp.size # 1) & (tp.size # 2)
					& (tp.size # 4) & (tp.size # 8)
				THEN cls := Base.cRef
				END;
				ronly := tp.form IN {Base.tArray, Base.tNRecord}
			END;
			obj := first;
			WHILE obj # NIL DO
				obj.class := cls;
				obj.readOnly := ronly;
				obj.val := parblksize;
				obj.type := tp;
				parblksize := parblksize + size; INC(nopar);
				obj := obj.next
			END;
			IF sym = Scanner.semicolon THEN NextSym;
				IF sym = Scanner.rparen THEN Error(superfluousSemicolonError)
				ELSIF (sym # Scanner.ident) & (sym # Scanner.var) THEN
					Error('Param declaration?')
				END
			END
		UNTIL (sym # Scanner.ident) & (sym # Scanner.var);
		proc.fields := SymTable.topScope.next; SymTable.CloseScope;
		proc.parblksize := parblksize; proc.nopar := nopar;
		Check(Scanner.rparen, noRParenError);
		IF sym = Scanner.colon THEN NextSym; qualident(obj);
			IF obj.class = Base.cType THEN
				proc.base := obj.type; size := obj.type.size;
				IF (size # 1) & (size # 2) & (size # 4) & (size # 8) THEN
					Error('Invalid return type'); proc.base := Base.intType
				END
			ELSE Error(notTypeError)
			END
		END
	END
END FormalParameters;

PROCEDURE NewProcedureType(VAR tp: Base.Type);
BEGIN Base.NewType(tp, Base.tProcedure);
	tp.size := Base.WordSize; tp.align := Base.WordSize;
	tp.parblksize := 0; tp.nopar := 0;
	IF sym = Scanner.lparen THEN FormalParameters(tp) END
END NewProcedureType;

PROCEDURE type(VAR tp: Base.Type);
	VAR obj: Base.Object;
		x: Base.Item;
		tpArray: Base.Type;
		undef: UndefPtrList;
		id: Base.IdStr;
		
	PROCEDURE CalculateArraySize(tp, tp2: Base.Type);
	BEGIN
		IF tp # tp2 THEN CalculateArraySize(tp.base, tp2) END;
		tp.size := tp.base.size * tp.len;
		tp.align := tp.base.align
	END CalculateArraySize;
	
BEGIN (* type *)
	tp := Base.intType;
	IF sym = Scanner.ident THEN qualident(obj);
		IF obj.class = Base.cType THEN
			IF (obj # defObj) OR (obj.type.form = Base.tNPointer) THEN
				tp := obj.type
			ELSE Error('Circular definition')
			END
		ELSE Error(notTypeError)
		END
	ELSIF sym = Scanner.array THEN NextSym;
		expression(x); CheckArrayLen(x);
		Base.NewType(tpArray, Base.tArray);
		tpArray.len := x.a; tp := tpArray;
		WHILE sym = Scanner.comma DO NextSym;
			expression(x); CheckArrayLen(x);
			Base.NewType(tpArray.base, Base.tArray);
			tpArray := tpArray.base; tpArray.len := x.a
		END;
		Check(Scanner.of, noOfError);
		type(tpArray.base); CalculateArraySize(tp, tpArray)
	ELSIF sym = Scanner.record THEN NextSym;
		Base.NewType(tp, Base.tNRecord); SymTable.OpenScope('');
		tp.size := 0; tp.align := 0;		
		IF sym = Scanner.semicolon THEN
			Error(superfluousSemicolonError); NextSym
		END;
		REPEAT
			IF sym = Scanner.ident THEN FieldList(tp)
			ELSIF sym = Scanner.union THEN Union(tp)
			END;
			IF sym = Scanner.semicolon THEN NextSym;
				IF sym = Scanner.end THEN Error(superfluousSemicolonError)
				ELSIF (sym # Scanner.ident) OR (sym # Scanner.union) THEN
					Error(notFieldListError)
				END
			END
		UNTIL (sym # Scanner.ident) & (sym # Scanner.union);
		tp.fields := SymTable.topScope.next; SymTable.CloseScope;
		Check(Scanner.end, noEndError)
	ELSIF sym = Scanner.pointer THEN
		NextSym; Check(Scanner.to, noToError);
		Base.NewType(tp, Base.tNPointer); tp.base := Base.intType;
		tp.size := Base.WordSize; tp.align := Base.WordSize;
		IF sym = Scanner.ident THEN
			id := Scanner.id; obj := SymTable.universe.next;
			WHILE (obj # NIL) & (obj.id # id) DO obj := obj.next END;
			IF obj # NIL THEN
				IF obj.class = Base.cType THEN
					IF obj.type.form = Base.tNRecord THEN tp.base := obj.type
					ELSE Error(notRecordTypeError)
					END
				ELSE Error(notTypeError)
				END
			ELSE NEW(undef); undef.tp := tp; undef.name := id;
				undef.next := undefList; undefList := undef
			END;
		ELSIF sym = Scanner.record THEN type(tp.base)
		END
	ELSIF sym = Scanner.procedure THEN
		NextSym; NewProcedureType(tp)
	END
END type;

PROCEDURE DeclarationSequence(VAR varsize: INTEGER);
	VAR id: Base.IdStr;
		x: Base.Item;
		first, obj, proc, param: Base.Object;
		typ: Base.Type;
		n, locblksize: INTEGER;
BEGIN
	IF sym = Scanner.const THEN NextSym;
		WHILE sym = Scanner.ident DO
			id := Scanner.id; NextSym;
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
			type(obj.type); Check(Scanner.semicolon, noSemicolonError)
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
					IF first = NIL THEN first := obj END;
					NextSym
				ELSE Error(superfluousCommaError)
				END
			END;
			Check(Scanner.colon, noColonError);
			type(typ); obj := first;
			WHILE obj # NIL DO
				obj.type := typ;
				obj.lev := SymTable.curLev;
				n := varsize - typ.size; n := n - n MOD typ.align;
				obj.val := n; varsize := n;
				obj := obj.next
			END;
			Check(Scanner.semicolon, noSemicolonError)
		END
	END;
	
	WHILE sym = Scanner.procedure DO NextSym;
		IF sym = Scanner.ident THEN id := Scanner.id;
			SymTable.New(proc, id, Base.cProc);
			IF proc # NIL THEN
				NextSym; NewProcedureType(proc.type);
				Check(Scanner.semicolon, noSemicolonError);
				SymTable.OpenScope(id); param := proc.type.fields;
				WHILE param # NIL DO
					SymTable.New(obj, param.name, Base.cVar);
					obj^ := param^; obj.next := NIL;
					param := param.next
				END;
				DeclarationSequence(locblksize);
				Generator.Enter;
				IF sym = Scanner.begin THEN NextSym; StatementSequence END;
				IF sym = Scanner.return THEN
					IF proc.type.base = NIL THEN
						Error('Proper procedure cannot have RETURN')
					END;
					NextSym; expression(x);
					IF ~CompType(x.type, proc.type.base) THEN
						Error(notCompTypeError)
					END
				ELSIF proc.type.base # NIL THEN
					Error('Function procedure need RETURN clause')
				END;
				Generator.Return;
				Check(Scanner.end, noEndError);
				IF sym = Scanner.ident THEN
					IF Scanner.id # id THEN Error('Wrong procedure name') END
				ELSE Error('No procedure name at the end')
				END
			ELSE WHILE sym < Scanner.const DO NextSym END
			END		
		ELSE Error(noIdentError);
			WHILE sym < Scanner.const DO NextSym END
		END
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
	type0 := type
END Parser0.