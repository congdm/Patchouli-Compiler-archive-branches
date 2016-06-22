MODULE Parser0;

IMPORT
	Base := Base0,
	Scanner := Scanner,
	SymTable := SymTable0,
	Generator := Generator0;

CONST
	noSemicolonError = 'No ;';
	noColonError = 'No :';
	noEqlError = 'No =';
	noEndError = 'No END';
	noOfError = 'No OF';
	
	notConstError = 'Not a const';
	notTypeError = 'Not a type';
	notIntType = 'Not an integer';
	
	superflousCommaError = 'Superflous ,';
	
VAR
	sym*: INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Check(expected: INTEGER; err: ARRAY OF CHAR);
BEGIN IF sym = expected THEN Scanner.Get(sym) ELSE Scanner.Mark(err) END
END Check;

PROCEDURE MakeIntConst(VAR x: Base.Item);
END MakeIntConst;

PROCEDURE CheckInt(VAR x: Base.Item);
BEGIN
	IF x.type.form # Base.tInt THEN
		Error(notIntType); x.type := Base.intType
	END
END CheckInt;

PROCEDURE CheckArrayLen(VAR x: Base.Item);
BEGIN CheckInt(x);
	IF x.mode # Base.cConst THEN Error(notConstError); x.a := 1
	ELSIF x.a < 1 THEN Error('Invalid array length'); x.a := 1
	END
END CheckArrayLen;

PROCEDURE NextSym;
BEGIN Scanner.Get(sym)
END NextSym;
	
PROCEDURE Error(err: INTEGER);
BEGIN Scanner.Mark(err)
END Error;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE expression(VAR x: Base.Item);
BEGIN
END expression;

PROCEDURE CalculateArraySize(tp, tp2: Base.Type);
BEGIN
	IF tp # tp2 THEN CalculateArraySize(tp.base, tp2) END;
	tp.size := tp.base.size * tp.len
END CalculateArraySize;

PROCEDURE FieldList(tp: Base.Type);
	VAR first, field: Base.Object;
		fieldType: Base.Type;
		n: INTEGER;
BEGIN
	SymTable.New(first, Scanner.id, Base.cField);
	WHILE sym = Scanner.comma DO NextSym;
		IF sym = Scanner.ident THEN
			SymTable.New(field, Scanner.id, Base.cField);
			IF first = Base.guard THEN first := field END;
			NextSym
		ELSE Error(superflousCommaError)
		END
	END;
	Check(Scanner.colon, noColonError);
	type(fieldType); field := first;
	n := tp.size; n := n - n MOD fieldType.alignment;
	IF n < tp.size THEN n := n + fieldType.alignment END;
	WHILE obj # Base.guard DO
		field.type := fieldType;
		field.lev := SymTable.curLev;
		field.val := n; n := n + fieldType.size; tp.size := n;
		field := field.next
	END;
END FieldList;

PROCEDURE type(VAR tp: Base.Type);
	VAR obj: Base.Object; x: Base.Item;
		tpArray: Base.Type;
BEGIN tp := Base.intType;
	IF sym = Scanner.ident THEN
		qualident(obj);
		IF obj.class = Base.cType THEN tp := obj.type
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
		Base.NewType(tp, Base.tnRecord);
		
	END
END type;

PROCEDURE DeclarationSequence(VAR varsize: INTEGER);
	VAR id: Base.IdStr; x: Base.Item;
		first, obj: Base.Object; typ: Base.Type;
		n: INTEGER;
BEGIN
	IF sym = Scanner.const THEN NextSym;
		WHILE sym = Scanner.ident DO
			id := Scanner.id; NextSym;
			Check(Scanner.eql, noEqlError); expression(x);
			IF x.mode # Base.cConst THEN
				Error(notConstError); MakeIntConst(x)
			END;
			SymTable.New(obj, id, x.mode);
			IF obj # Base.guard THEN
				obj.type := x.type; obj.val := x.val; obj.lev := x.lev
			END;
			Check(Scanner.semicolon, noSemicolonError)
		END
	END;
	
	IF sym = Scanner.type THEN NextSym;
	END;
	
	varsize := 0;
	IF sym = Scanner.var THEN NextSym;
		WHILE sym = Scanner.ident DO
			SymTable.New(first, Scanner.id, Base.cVar);
			WHILE sym = Scanner.comma DO NextSym;
				IF sym = Scanner.ident THEN
					SymTable.New(obj, Scanner.id, Base.cVar);
					IF first = Base.guard THEN first := obj END;
					NextSym
				ELSE Error(superflousCommaError)
				END
			END;
			Check(Scanner.colon, noColonError);
			type(typ); obj := first;
			WHILE obj # Base.guard DO
				obj.type := typ;
				obj.lev := SymTable.curLev;
				n := varsize - typ.size; n := n - n MOD typ.alignment;
				obj.val := n; varsize := n;
				obj := obj.next
			END;
			Check(Scanner.semicolon, noSemicolonError)
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
		SymTable.Init(modid); Generator.Init(modid);
		(* IF sym = Scanner.import THEN ImportList END; *)
		IF Scanner.errcnt = 0 THEN
			varsize := 0; DeclarationSequence(varsize);
			
			Generator.Enter;
			IF sym = Scanner.begin THEN NextSym; StatementSequence END;
			Generator.Return;
			
			Check(Scanner.end, noEndError);
			IF sym = Scanner.ident THEN
				IF modid # Scanner.id THEN Error('Wrong module name')
				END;
				NextSym
			ELSE Error('No module identifier')
			END;
			Check(Scanner.period, 'No ending .');
			Generator.Finish
		END
	END
END Module;

BEGIN
END Parser0.