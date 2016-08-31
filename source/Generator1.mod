MODULE Generator1;

IMPORT
	SYS := SYSTEM, B := Base1, S := Scanner1;
	
PROCEDURE FoldConst*(op: INTEGER; x, y: B.Object): B.Object;
	VAR val, xval, yval: INTEGER; type: B.Type;
BEGIN
	IF (x IS B.Const) & (y IS B.Const) & (x.type # B.realType) THEN
		xval := x(B.Const).val; yval := y(B.Const).val;
		IF op >= S.eql THEN
			IF (op = S.eql) & (xval = yval) OR (op = S.neq) & (xval # yval)
			OR (op = S.gtr) & (xval > yval) OR (op = S.geq) & (xval >= yval)
			OR (op = S.lss) & (xval < yval) OR (op = S.leq) & (xval <= yval)
			OR (op = S.in) & (xval IN SYS.VAL(SET,yval))
			THEN val := 1
			ELSE val := 0
			END; type := B.boolType
		END
	END;
	x := B.NewConst(type, val);
	RETURN x
END FoldConst;

END Generator1.