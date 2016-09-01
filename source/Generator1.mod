MODULE Generator1;

IMPORT
	SYS := SYSTEM, B := Base1, S := Scanner1;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: B.Object): B.Object;
	VAR val, xval, yval: INTEGER; type: B.Type;
		r1, r2: REAL;
BEGIN
	IF (x IS B.Const) & (y IS B.Const) & (x.type # B.realType) THEN
		xval := x(B.Const).val; yval := y(B.Const).val;
		IF op >= S.eql THEN
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYS.VAL(SET,yval))
			THEN val := 1 ELSE val := 0
			END; type := B.boolType
		END
	ELSIF x.type = B.realType THEN
		xval := x(B.Const).val; yval := y(B.Const).val;
		r1 := SYS.VAL(REAL, xval); r2 := SYS.VAL(REAL, yval);
		IF op >= S.eql THEN
			IF (op = S.eql) & (r1 = r2) OR (op = S.neq) & (r1 # r2)
			OR (op = S.gtr) & (r1 > r2) OR (op = S.geq) & (r1 >= r2)
			OR (op = S.lss) & (r1 < r2) OR (op = S.leq) & (r1 <= r2)
			THEN val := 1 ELSE val := 0
			END; type := B.boolType
		END
	END;
	x := B.NewConst(type, val);
	RETURN x
END FoldConst;

END Generator1.