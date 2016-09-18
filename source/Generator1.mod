MODULE Generator1;

IMPORT
	SYS := SYSTEM,
	Crypt, BaseSys,
	S := Scanner1, B := Base1;

CONST
	MaxInt = 9223372036854775807;
	MinInt = -MaxInt-1;	
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Const folding *)
	
PROCEDURE CheckSetElement*(x: B.Object);
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val ELSE val := 0 END;
	IF (val < 0) OR (val > 63) THEN
		S.Mark('Set element must be >= 0 and <= 63')
	END
END CheckSetElement;

PROCEDURE SingletonSet*(x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val MOD 64 ELSE val := 0 END;
	x := B.NewConst(B.setType, ORD({val}));
	RETURN x
END SingletonSet;

PROCEDURE RangeSet*(x, y: B.Object): B.Object;
	VAR beg, end: INTEGER;
BEGIN
	IF x IS B.Const THEN beg := x(B.Const).val MOD 64 ELSE beg := 0 END;
	IF y IS B.Const THEN end := y(B.Const).val MOD 64 ELSE end := 0 END;
	x := B.NewConst(B.setType, ORD({beg..end}));
	RETURN x
END RangeSet;

PROCEDURE MultiplyConst(x, y: INTEGER): INTEGER;
	VAR u, v: INTEGER; q, r: INTEGER;
BEGIN
	IF (x > 0) & (y > 0) THEN
		IF MaxInt DIV y < x THEN S.Mark('arithmetic overflow')
		ELSE x := x * y
		END
	ELSIF (x = 0) OR (y = 0) THEN x := 0
	ELSIF (x < 0) & (y < 0) THEN x := MultiplyConst(-x, -y)
	ELSE
		IF (x < 0) THEN u := y; v := x ELSE u := x; v := y END;
		q := MinInt DIV u; r := MinInt MOD u;
		IF v > q THEN x := u * v
		ELSIF (v = q) & (r = 0) THEN x := MinInt
		ELSE S.Mark('arithmetic overflow')
		END
	END;
	RETURN x
END MultiplyConst;

PROCEDURE NegateConst*(x0: B.Object): B.Const;
	VAR x: B.Const; type: B.Type; val: INTEGER;
BEGIN
	type := x0.type; IF x0 IS B.Const THEN val := x0(B.Const).val END;
	x := B.NewConst(type, val);
	IF x.type = B.byteType THEN x.type := B.intType END;
	IF x.type = B.intType THEN x.val := -x.val
	ELSIF x.type = B.realType THEN
		x.val := SYS.VAL(INTEGER, -SYS.VAL(REAL, x.val))
	ELSIF x.type = B.setType THEN x.val := ORD(-SYS.VAL(SET, x.val))
	ELSIF x.type = B.boolType THEN x.val := (x.val + 1) MOD 2
	END;
	RETURN x
END NegateConst;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: B.Object): B.Object;
	VAR val, xval, yval, i, k: INTEGER; type: B.Type;
		r1, r2: REAL; xstr, ystr: B.Str; ch1, ch2: CHAR;
BEGIN
	IF (op >= S.eql) & (op <= S.in) THEN
		IF (x IS B.Const) & (y IS B.Const) & (x.type # B.realType) THEN
			xval := x(B.Const).val; yval := y(B.Const).val;
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYS.VAL(SET,yval))
			THEN val := 1 ELSE val := 0
			END
		ELSIF (x IS B.Const) & (y IS B.Const) & (x.type = B.realType) THEN
			xval := x(B.Const).val; yval := y(B.Const).val;
			r1 := SYS.VAL(REAL, xval); r2 := SYS.VAL(REAL, yval);
			IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
			OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
			OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
			THEN val := 1 ELSE val := 0
			END
		ELSIF (x IS B.Str) & (y IS B.Str) THEN
			xstr := x(B.Str); ystr := y(B.Str);
			IF (xstr.bufpos >= 0) & (ystr.bufpos >= 0) THEN
				i := xstr.bufpos; k := ystr.bufpos;
				ch1 := B.strbuf[i]; ch2 := B.strbuf[k];
				WHILE (ch1 = ch2) & (ch1 # 0X) DO
					INC(i); INC(k); ch1 := B.strbuf[i]; ch2 := B.strbuf[k] 
				END;
				IF (op = S.eql) & (ch1 = ch2) OR (op = S.neq) & (ch1 # ch2)
				OR (op = S.gtr) & (ch1 > ch2) OR (op = S.geq) & (ch1 >= ch2)
				OR (op = S.lss) & (ch1 < ch2) OR (op = S.leq) & (ch1 <= ch2)
				THEN val := 1 ELSE val := 0
				END 
			END
		END;
		type := B.boolType
	ELSIF (x IS B.Const) & (y IS B.Const) THEN
		xval := x(B.Const).val; yval := y(B.Const).val;
		IF x.type.form = B.tInt THEN type := B.intType;
			IF op = S.plus THEN
				IF yval > MaxInt-xval THEN S.Mark('arithmetic overflow')
				ELSE val := xval + yval
				END
			ELSIF op = S.minus THEN
				IF MinInt+yval < xval THEN S.Mark('arithmetic overflow')
				ELSE val := xval - yval
				END
			ELSIF op = S.times THEN val := MultiplyConst(xval, yval)
			ELSIF (op = S.div) OR (op = S.mod) THEN
				IF yval <= 0 THEN S.Mark('invalid divisor')
				ELSIF op = S.div THEN val := xval DIV yval
				ELSE val := xval MOD yval
				END
			END
		ELSIF x.type = B.setType THEN type := B.setType;
			IF op = S.plus THEN
				val := ORD(SYS.VAL(SET, xval) + SYS.VAL(SET, yval))
			ELSIF op = S.minus THEN
				val := ORD(SYS.VAL(SET, xval) - SYS.VAL(SET, yval))
			ELSIF op = S.times THEN
				val := ORD(SYS.VAL(SET, xval) * SYS.VAL(SET, yval))
			ELSIF op = S.rdiv THEN
				val := ORD(SYS.VAL(SET, xval) / SYS.VAL(SET, yval))
			END
		ELSIF x.type = B.realType THEN type := B.realType;
			r1 := SYS.VAL(REAL, xval); r2 := SYS.VAL(REAL, yval);
			IF op = S.plus THEN val := SYS.VAL(INTEGER, r1 + r2)
			ELSIF op = S.minus THEN val := SYS.VAL(INTEGER, r1 - r2)
			ELSIF op = S.times THEN val := SYS.VAL(INTEGER, r1 * r2)
			ELSIF op = S.rdiv THEN
				IF (r2 # 0.0) & (r2 # -0.0) THEN
					val := SYS.VAL(INTEGER, r1 / r2)
				ELSE S.Mark('division by zero')
				END
			END
		ELSIF x.type = B.boolType THEN type := B.boolType;
			IF op = S.or THEN
				IF (xval = 1) OR (yval = 1) THEN val := 1 ELSE val := 0 END
			ELSIF op = S.and THEN
				IF (xval = 1) & (yval = 1) THEN val := 1 ELSE val := 0 END
			END
		END
	END;
	x := B.NewConst(type, val);
	RETURN x
END FoldConst;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Generate*(modid: B.IdStr; modinit: B.Node);
BEGIN
END Generate;

END Generator1.