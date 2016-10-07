MODULE Generator1;

IMPORT
	SYSTEM, BaseSys,
	S := Scanner1, B := Base1;

CONST
	MaxInt = 9223372036854775807;
	MinInt = -MaxInt-1;
	
	MaxSize = 80000000H; (* 2 GB limit *)
	MaxLocBlkSize = 100000H; (* 1 MB limit *)
	
	reg_A = 0; reg_C = 1; reg_D = 2; reg_B = 3;
	reg_SP = 4; reg_BP = 5; reg_SI = 6; reg_DI = 7;
	reg_R8 = 8; reg_R9 = 9; reg_R10 = 10; reg_R11 = 11;
	reg_R12 = 12; reg_R13 = 13; reg_R14 = 14; reg_R15 = 15;
	
	ccO = 0; ccNO = 1; ccB = 2; ccAE = 3; ccZ = 4; ccNZ = 5; ccBE = 6; ccA = 7;
	ccS = 8; ccNS = 9; ccP = 10; ccNP = 11; ccL = 12; ccGE = 13; ccLE = 14;
	ccG = 15; ccAlways = 16; ccNever = 17;
	ccC = ccB; ccNC = ccAE;
	
	(* Opcodes used with EmitRegRm *)
	ADD = 00H; ADDd = 02H; AND = 20H; ANDd = 22H; XOR = 30H; XORd = 32H;
	TEST = 84H; XCHG = 86H; 
	OR_ = 08H; ORd = 0AH; SUB = 28H; SUBd = 2AH; CMP = 38H; CMPd = 3AH;
	MOV = 88H; MOVd = 8AH; LEA = 8DH;
	BT = 0A30FH; BTR = 0B30FH;
	BTS = 0AB0FH; IMUL = 0AF0FH;
	
	(* Opcodes used with EmitRm *)
	POP = 8FH; ROR1 = 1D0H; RORcl = 1D2H; SHL1 = 4D0H; SHLcl = 4D2H;
	SHR1 = 5D0H; SHRcl = 5D2H; SAR1 = 7D0H; SARcl = 7D2H;
	NOT = 2F6H; NEG = 3F6H; IDIVa = 7F7H; _INC = 0FEH; _DEC = 1FEH;
	CALL = 2FFH; JMP = 4FFH; PUSH = 6FFH;
	LDMXCSR = 2AE0FH; STMXCSR = 3AE0FH;
	
	(* Opcodes used with EmitRmImm *)
	ADDi = 80H; ORi = 180H; ANDi = 480H; SUBi = 580H; XORi = 680H; CMPi = 780H;
	RORi = 1C0H; SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; MOVi = 0C6H;
	TESTi = 76H; BTi = 4BA0FH; BTSi = 5BA0FH; BTRi = 6BA0FH; BTCi = 7BA0FH;
	IMULi = 69H (* Special case *);
	
	(* Opcodes used with EmitBare *)
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H; INT3 = 0CCH;
	
	(* REP instructions *)
	MOVSrep = 0A4H;
	
	(* Opcodes used with EmitXmmRm *)
	SeeMOVD = 6E0F66H; SeeMOVDd = 7E0F66H; MOVSS = 100FF3H; MOVSSd = 110FF3H;
	ADDSS = 580FF3H; MULSS = 590FF3H; SUBSS = 5C0FF3H; DIVSS = 5E0FF3H;
	ADDPS = 580F00H; MULPS = 590F00H; SUBPS = 5C0F00H; DIVPS = 5E0F00H;
	ANDPS = 540F00H; ANDNPS = 550F00H; ORPS = 560F00H; XORPS = 570F00H;
	MOVAPS = 280F00H; MOVAPSd = 290F00H; COMISS = 2F0F00H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	
	mReg = 0; mXReg = 1; mImm = 2; mRegI = 3; mIP = 4; mSP = 5; mBP = 6;
	mProc = 7; mType = 8;
	
TYPE
	Proc = POINTER TO RECORD
		usedRegs: SET; adr, parWindowSize: INTEGER;
		prologue, epilogue, body, prologSize, epilogSize, bodySize: INTEGER;
		regSavingStackSize, regSavingStackPos: INTEGER;
		next: Proc
	END;
	
	Inst = POINTER TO EXTENSIBLE RECORD next, link: Inst; sz: BYTE END;
	Inst1 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 1 OF BYTE END;
	Inst2 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 2 OF BYTE END;
	Inst3 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 3 OF BYTE END;
	Inst4 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 4 OF BYTE END;
	Inst5 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 5 OF BYTE END;
	Inst6 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 6 OF BYTE END;
	Inst7 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 7 OF BYTE END;
	Inst8 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 8 OF BYTE END;
	Inst9 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 9 OF BYTE END;
	Inst10 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 10 OF BYTE END;
	Inst11 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 11 OF BYTE END;
	Inst12 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 12 OF BYTE END;
	Inst13 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 13 OF BYTE END;
	Inst14 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 14 OF BYTE END;
	Inst15 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 15 OF BYTE END;
	Inst16 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 16 OF BYTE END;
	Inst17 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 17 OF BYTE END;
	Inst18 = POINTER TO RECORD EXTENSIBLE (Inst) a: ARRAY 18 OF BYTE END;
	
	Node = POINTER TO RECORD
		mode, op, r, rm: BYTE; ref: BOOLEAN;
		a, b: INTEGER; mustUseRegs: SET; type: B.Type;
		x, y, next, fLink, tLink: Node
	END
	
VAR
	code: ARRAY 32 OF BYTE; cpos: INTEGER;
	procList, curProc: Proc;

	stack, regStack: INTEGER;
	mem: RECORD
		mod, rm, bas, idx, scl, disp: INTEGER
	END;
	
	varSize, staticSize: INTEGER;
	impList: B.Ident;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)

PROCEDURE Put1(n: INTEGER);
BEGIN code[cpos] := n MOD 256; INC(cpos)
END Put1;

PROCEDURE Put2(n: INTEGER);
BEGIN
	code[cpos] := n MOD 256; n := n DIV 256;
	code[cpos+1] := n MOD 256; cpos := cpos + 2
END Put2;

PROCEDURE Put4(n: INTEGER);
BEGIN
	code[cpos] := n MOD 256; n := n DIV 256;
	code[cpos+1] := n MOD 256; n := n DIV 256;
	code[cpos+2] := n MOD 256; n := n DIV 256;
	code[cpos+3] := n MOD 256; cpos := cpos + 4
END Put4;

PROCEDURE Put8(n: INTEGER);
BEGIN
	code[cpos] := n MOD 256; n := n DIV 256;
	code[cpos+1] := n MOD 256; n := n DIV 256;
	code[cpos+2] := n MOD 256; n := n DIV 256;
	code[cpos+3] := n MOD 256; n := n DIV 256;
	code[cpos+4] := n MOD 256; n := n DIV 256;
	code[cpos+5] := n MOD 256; n := n DIV 256;
	code[cpos+6] := n MOD 256; n := n DIV 256;
	code[cpos+7] := n MOD 256; cpos := cpos + 8
END Put8;
	
PROCEDURE EmitREX(reg, rsize: INTEGER);
	CONST W = 8; R = 4; X = 2; B = 1;
	VAR rex: INTEGER;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W END;
	IF reg >= reg_R8 THEN rex := rex + R END;
	IF (mem.rm >= reg_R8)
	OR (mem.mod # 3) & (mem.rm = reg_SP) & (mem.bas >= reg_R8)
	THEN rex := rex + B
	END;
	IF (mem.mod # 3) & (mem.rm = reg_SP) & (mem.idx >= reg_R8)
	THEN rex := rex + X
	END;
	IF (rex # 40H)
	OR (rsize = 1)
		& ((reg IN {reg_SP..reg_DI})
		OR (mem.mod = 3) & (mem.rm IN {reg_SP..reg_DI}))
	THEN Put1(rex)
	END
END EmitREX;

PROCEDURE Emit16bitPrefix(rsize: INTEGER);
BEGIN IF rsize = 2 THEN Put1(66H) END
END Emit16bitPrefix;

PROCEDURE HandleMultibytesOpcode(VAR op: INTEGER);
BEGIN
	IF op MOD 256 = 0FH THEN
		Put1(0FH); op := op DIV 256;
		IF (op MOD 256 = 38H) OR (op MOD 256 = 3AH) THEN
			Put1(op); op := op DIV 256
		END
	END
END HandleMultibytesOpcode;

PROCEDURE EmitModRM(reg: INTEGER);
BEGIN
	Put1(mem.mod * 64 + reg MOD 8 * 8 + mem.rm MOD 8);
	IF mem.mod # 3 THEN
		IF mem.rm IN {reg_SP, reg_R12} THEN
			Put1(mem.scl * 64 + mem.idx MOD 8 * 8 + mem.bas MOD 8)
		END;
		IF (mem.mod = 0) & (mem.rm IN {reg_BP, reg_R13})
		OR (mem.mod = 2)
		THEN Put4(mem.disp)
		ELSIF mem.mod = 1 THEN Put1(mem.disp)
		END
	END
END EmitModRM;

(* -------------------------------------------------------------------------- *)

PROCEDURE IntToSet(n: INTEGER): SET;
	RETURN SYSTEM.VAL(SET, n)
END IntToSet;
	
PROCEDURE EmitRegRm(op, reg, rsize: INTEGER);
	CONST w = 1;
	VAR org: INTEGER;
BEGIN
	Emit16bitPrefix(rsize); EmitREX(reg, rsize);
	org := op; HandleMultibytesOpcode(op);
	
	IF (rsize > 1) & (org < LEA) THEN op := op + w END;
	Put1(op); EmitModRM(reg)
END EmitRegRm;

PROCEDURE EmitRm (op, rsize: INTEGER);
	CONST w = 1;
	VAR op3bits, org: INTEGER;
BEGIN
	Emit16bitPrefix(rsize); EmitREX(0, rsize);
	org := op; HandleMultibytesOpcode(op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF (rsize > 1) & ~ODD(op) & (org # LDMXCSR) & (org # STMXCSR)
	THEN op := op + w
	END;
	Put1(op); EmitModRM(op3bits)
END EmitRm;

PROCEDURE EmitRmImm (op, rsize, imm: INTEGER);
	CONST w = 1; s = 2;
	VAR op3bits: INTEGER;
BEGIN
	Emit16bitPrefix(rsize);
	IF op MOD 256 # IMULi THEN EmitREX(0, rsize)
	ELSE EmitREX(op DIV 256, rsize)
	END;
	HandleMultibytesOpcode(op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF rsize > 1 THEN
		IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
		ELSIF (imm >= -128) & (imm <= 127) & (op = 80H) THEN
			op := op + s; rsize := 1
		END;
		IF ~ODD(op) & (op # 0BAH) THEN op := op + w END
	END;
	Put1(op); EmitModRM(op3bits);
	
	IF rsize = 1 THEN Put1(imm)
	ELSIF rsize = 2 THEN Put2(imm) ELSE Put4(imm)
	END
END EmitRmImm;

PROCEDURE EmitBare(op: INTEGER);
BEGIN WHILE op > 0 DO Put1(op); op := op DIV 256 END
END EmitBare;

PROCEDURE EmitXmmRm(op, xreg, rsize: INTEGER);
	VAR prefix: INTEGER;
BEGIN
	prefix := op MOD 256; op := op DIV 256;
	IF prefix # 0 THEN Put1(prefix) END;
	EmitREX(xreg, rsize); HandleMultibytesOpcode(op);
	Put1(op MOD 256); EmitModRM(xreg)
END EmitXmmRm;

PROCEDURE EmitMOVZX(reg, rmsize: INTEGER);
	VAR rsize, op: INTEGER;
BEGIN rsize := 4; op := 0B6H;
	IF rmsize = 1 THEN
		IF (mem.mod = 3) & (mem.rm IN {reg_SP..reg_DI})
		THEN rsize := 8
		END
	ELSIF rmsize = 2 THEN INC(op)
	ELSE ASSERT(FALSE)
	END;
	EmitREX(reg, rsize); Put1(0FH); Put1(op); EmitModRM(reg)
END EmitMOVZX;

PROCEDURE EmitMOVSX(reg, rmsize: INTEGER);
	VAR op: INTEGER;
BEGIN
	IF rmsize = 1 THEN op := 0BE0FH
	ELSIF rmsize = 2 THEN op := 0BF0FH
	ELSIF rmsize = 4 THEN op := 63H
	ELSE ASSERT(FALSE)
	END;
	EmitREX(reg, 8); HandleMultibytesOpcode(op);
	Put1(op); EmitModRM(reg)
END EmitMOVSX;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRm_reg(reg: INTEGER);
BEGIN mem.rm := reg; mem.mod := 3
END SetRm_reg;

PROCEDURE SetRm_regI(reg, disp: INTEGER);
BEGIN
	mem.rm := reg; mem.disp := disp;
	IF (disp >= -128) & (disp <= 127) THEN
		IF (disp = 0) & ~(reg IN {reg_BP, reg_R13}) THEN mem.mod := 0
		ELSE mem.mod := 1
		END
	ELSE mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		mem.bas := reg_SP; mem.idx := reg_SP; mem.scl := 0
	END
END SetRm_regI;

(*PROCEDURE SetRmOperand_staticvar (adr: INTEGER);
BEGIN
	Emit.mem.rm := reg_BP; Emit.mem.disp := adr + staticbase;
	Emit.mem.mod := 0; metacode[pc].relfixup := TRUE
END SetRmOperand_staticvar;

PROCEDURE SetRmOperand (x : Base.Item);
BEGIN
	IF x.mode IN {Base.cVar, Base.cRef} THEN
		Emit.mem.rm := reg_BP; Emit.mem.disp := x.a;
		IF x.lev > 0 THEN
			IF (x.a >= -128) & (x.a <= 127) THEN Emit.mem.mod := 1
			ELSE Emit.mem.mod := 2
			END
		ELSE
			Emit.mem.mod := 0; metacode[pc].relfixup := TRUE;
			IF x.lev = 0 THEN Emit.mem.disp := Emit.mem.disp + varbase
			ELSE Emit.mem.disp := Emit.mem.disp + staticbase
			END
		END
	ELSIF x.mode = mRegI THEN SetRmOperand_regI (x.r, x.a)
	ELSIF x.mode = mReg THEN SetRmOperand_reg (x.r)
	ELSIF x.mode = Base.cProc THEN Emit.mem.rm := reg_BP; Emit.mem.disp := x.a;
		Emit.mem.mod := 0; metacode[pc].relfixup := TRUE;
		IF x.lev < 0 THEN Emit.mem.disp := Emit.mem.disp + staticbase END
	END
END SetRmOperand;*)

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR(op, reg, rsize, rm: INTEGER);
BEGIN SetRm_reg(rm); EmitRegRm(op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI(op, rm, rsize, imm: INTEGER);
BEGIN
	SetRm_reg(rm); IF op = IMULi THEN op := op + rm * 256 END;
	EmitRmImm(op, rsize, imm)
END EmitRI;

PROCEDURE EmitR(op, rm, rsize: INTEGER);
BEGIN SetRm_reg(rm); EmitRm(op, rsize)
END EmitR;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI(rm, rsize, imm: INTEGER);
	CONST w = 8;
	VAR op: INTEGER;
BEGIN
	SetRm_reg(rm); Emit16bitPrefix(rsize);
	EmitREX(0, rsize); op := 0B0H + rm MOD 8;
	IF rsize > 1 THEN op := op + w END; Put1(op);
	IF rsize = 1 THEN Put1(imm) ELSIF rsize = 2 THEN Put2(imm)
	ELSIF rsize = 4 THEN Put4(imm) ELSE Put8(imm)
	END
END MoveRI;

PROCEDURE PushR(rm: INTEGER);
BEGIN
	SetRm_reg (rm); EmitREX(0, 4); Put1(50H + rm MOD 8); stack := stack + 8
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN
	SetRm_reg (rm); EmitREX(0, 4); Put1(58H + rm MOD 8); stack := stack - 8
END PopR;

PROCEDURE Branch(disp: INTEGER);
BEGIN Put1(0E9H); Put4(disp)
END Branch;

PROCEDURE CallNear(disp: INTEGER);
BEGIN Put1(0E8H); Put4(disp)
END CallNear;

PROCEDURE CondBranch(cond, disp: INTEGER);
BEGIN Put1(0FH); Put1(80H + cond); Put4(disp)
END CondBranch;

PROCEDURE SetccRm(cond: INTEGER);
BEGIN EmitREX(0, 1); Put1(0FH); Put1(90H + cond); EmitModRM(0)
END SetccRm;

PROCEDURE EmitRep(op, rsize, z: INTEGER);
	CONST w = 1;
BEGIN
	Put1(0F2H + z); (* REP prefix *)
	Emit16bitPrefix(rsize); EmitREX(0, rsize);
	IF (rsize > 1) & ~ODD(op) THEN op := op + w END;
	Put1(op)
END EmitRep;
	
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
		x.val := SYSTEM.VAL(INTEGER, -SYSTEM.VAL(REAL, x.val))
	ELSIF x.type = B.setType THEN x.val := ORD(-SYSTEM.VAL(SET, x.val))
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
			OR (op = S.in) & (xval IN SYSTEM.VAL(SET,yval))
			THEN val := 1 ELSE val := 0
			END
		ELSIF (x IS B.Const) & (y IS B.Const) & (x.type = B.realType) THEN
			xval := x(B.Const).val; yval := y(B.Const).val;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
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
				val := ORD(SYSTEM.VAL(SET, xval) + SYSTEM.VAL(SET, yval))
			ELSIF op = S.minus THEN
				val := ORD(SYSTEM.VAL(SET, xval) - SYSTEM.VAL(SET, yval))
			ELSIF op = S.times THEN
				val := ORD(SYSTEM.VAL(SET, xval) * SYSTEM.VAL(SET, yval))
			ELSIF op = S.rdiv THEN
				val := ORD(SYSTEM.VAL(SET, xval) / SYSTEM.VAL(SET, yval))
			END
		ELSIF x.type = B.realType THEN type := B.realType;
			r1 := SYSTEM.VAL(REAL, xval); r2 := SYSTEM.VAL(REAL, yval);
			IF op = S.plus THEN val := SYSTEM.VAL(INTEGER, r1 + r2)
			ELSIF op = S.minus THEN val := SYSTEM.VAL(INTEGER, r1 - r2)
			ELSIF op = S.times THEN val := SYSTEM.VAL(INTEGER, r1 * r2)
			ELSIF op = S.rdiv THEN
				IF (r2 # 0.0) & (r2 # -0.0) THEN
					val := SYSTEM.VAL(INTEGER, r1 / r2)
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
(* Set size, alignment, etc. for all types and variables *)

PROCEDURE MarkSizeError;
BEGIN S.Mark('Data size limit reached')
END MarkSizeError;

PROCEDURE SetTypeSize*(tp: B.Type);
	VAR size, align, fAlign, fSize: INTEGER; ident: B.Ident;
		ftype: B.Type;
BEGIN
	IF tp.size = 0 THEN
		IF tp.form = B.tPtr THEN tp.size := 8; tp.align := 8
		ELSIF tp.form = B.tProc THEN tp.size := 8; tp.align := 8;
			ident := tp.fields; size := 0;
			WHILE ident # NIL DO
				ftype := ident.obj.type; ident.obj(B.Var).adr := size;
				IF (ftype.form = B.tArray) & (ftype.len < 0)
				OR (ftype.form = B.tRec) & (ident.obj(B.Var).ref)
				THEN size := size + 16
				ELSE size := size + 8
				END;
				ident := ident.next
			END;
			tp.parblksize := size
		ELSIF (tp.form = B.tArray) & (tp.len >= 0) THEN
			SetTypeSize(tp.base); tp.align := tp.base.align;
			IF (tp.len # 0) & (MaxSize DIV tp.len < tp.base.size) THEN
				MarkSizeError; tp.size := tp.align
			ELSE tp.size := tp.base.size * tp.len
			END
		ELSIF tp.form = B.tRec THEN
			IF tp.base # NIL THEN SetTypeSize(tp.base);
				size := tp.base.size; align := tp.base.align
			ELSE size := 0; align := 0
			END;
			ident := tp.fields;
			WHILE ident # NIL DO
				SetTypeSize(ident.obj.type);
				fAlign := ident.obj.type.align; fSize := ident.obj.type.size;
				IF fAlign > align THEN align := fAlign END;
				size := size + (-size) MOD fAlign;
				IF MaxSize < size THEN MarkSizeError; size := align END;
				ident.obj(B.Field).off := size; size := size + fSize;
				ident := ident.next
			END;
			tp.size := size; tp.align := align
		ELSE ASSERT(FALSE)
		END
	END
END SetTypeSize;

PROCEDURE SetGlobalVarSize*(x: B.Object);
BEGIN
	varSize := varSize + (-varSize) MOD x.type.align;
	varSize := varSize + x.type.size; x(B.Var).adr := -varSize;
	IF varSize > MaxSize THEN varSize := 0; MarkSizeError END
END SetGlobalVarSize;

PROCEDURE SetProcVarSize*(proc: B.Proc; x: B.Object);
	VAR size: INTEGER;
BEGIN size := proc.locblksize;
	size := size + (-size) MOD x.type.align + x.type.size;
	x(B.Var).adr := -size; proc.locblksize := size;
	IF size > MaxLocBlkSize THEN proc.locblksize := 0; MarkSizeError END
END SetProcVarSize;

PROCEDURE AllocImportModules*;
	VAR i: INTEGER;
BEGIN
	FOR i := 0 TO B.modno-1 DO
		B.modList[i].adr := staticSize; staticSize := staticSize + 8
	END
END AllocImportModules;

PROCEDURE AllocImport*(x: B.Object);
	VAR p: B.Ident;
BEGIN NEW(p); p.obj := x; p.next := impList; impList := p;
	IF x IS B.Var THEN x(B.Var).adr := staticSize
	ELSIF x IS B.Proc THEN x(B.Proc).adr := staticSize
	ELSIF x.class = B.cType THEN x.type.adr := staticSize
	END;
	staticSize := staticSize + 8
END AllocImport;

PROCEDURE AllocStaticData;
	VAR p: B.Ident; q: B.TypeList; x: B.Object;
BEGIN p := B.strList;
	WHILE p # NIL DO x := p.obj; x(B.Str).adr := staticSize;
		staticSize := staticSize + 2*x(B.Str).len; p := p.next
	END;
	staticSize := staticSize + (-staticSize) MOD 8;
	q := B.recList;
	WHILE q # NIL DO q.type.adr := staticSize;
		staticSize := staticSize + 24 + 8 * (B.MaxExt + q.type.nptr);
		q := q.next
	END
END AllocStaticData;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InitItem(x: Item; obj: B.Object);
BEGIN x.usedReg := {}; x.codeSize := 0; x.obj := obj; x.type := obj.type
END InitItem;

PROCEDURE RefToRegI(VAR x: Node);
BEGIN IF x.ref THEN Deref(x) END
END RefToRegI;

PROCEDURE LoadVar(VAR x: Node);
BEGIN IF x.mode IN {mRegI, mIP, mBP, mSP} THEN load(x) END
END LoadVar;

PROCEDURE ToCond(VAR x: Node);
BEGIN
END ToCond;

PROCEDURE SymToOp(sym: INTEGER): INTEGER;
	RETURN sym
END SymToOp;

PROCEDURE MakeNode(x: B.Object): Node;
	VAR node: Node; sym, val: INTEGER;
BEGIN
	IF x = NIL THEN node := NIL
	ELSE NEW(node);
		node.type := x.type; node.mustUseRegs := {}; node.ref := FALSE;
		IF x IS B.Const THEN
			val := x(B.Const).val; node.mode := mImm; node.a := val
		ELSIF x IS B.Var THEN
			node.a := x(B.Var).adr;
			IF x(B.Var).lev = 0 THEN node.mode := mIP
			ELSIF x(B.Var).lev > 0 THEN
				node.mode := mSP; node.ref := x(B.Var).ref
			ELSIF x(B.Var).lev < -1 THEN node.mode := mIP; node.ref := TRUE
			ELSE ASSERT(FALSE);
			END
		ELSIF x IS B.Proc THEN
			node.mode := mProc; node.a := x(B.Proc).adr;
			node.ref := x(B.Proc).lev < -1
		ELSIF x.class = B.cType THEN
			node.mode := mType; node.a := x.type.adr;
			node.ref := x.type.lev < -1
		ELSIF x IS B.Node THEN
			sym := x(B.Node).op; node.op := SymToOp(sym);
			node.x := MakeNode(x(B.Node).left);
			node.y := MakeNode(x(B.Node).right);
			IF node.x # NIL THEN
				node.mustUseRegs := node.mustUseRegs + node.x.mustUseRegs
			END;
			IF node.y # NIL THEN
				node.mustUseRegs := node.mustUseRegs + node.y.mustUseRegs
			END;
			IF (sym >= S.times) & (sym <= S.mod)
			OR (sym = S.plus) OR (sym = S.minus) THEN
				LoadVar(node.x); LoadVar(node.y);
				IF pn.type # B.realType THEN node.mode := mReg;
					IF (sym = S.div) OR (sym = S.mod) THEN
						IF (node.y.mode # mImm) OR (log2(node.y.a) < 0) THEN 
							INCL(node.mustUseRegs, reg_A);
							INCL(node.mustUseRegs, reg_D)
						ELSIF log2(node.y.a) >= 2 THEN
							INCL(node.mustUseRegs, reg_C)
						END
					ELSIF sym = S.times THEN
						IF (node.y.mode = mImm) & (log2(node.y.a) >= 2)
						OR (node.x.mode = mImm) & (log2(node.x.a) >= 2)
						THEN INCL(node.mustUseRegs, reg_C)
						END
					END
				ELSE node.mode := mXReg
				END
			ELSIF (sym = S.and) OR (sym = S.or) THEN
				ToCond(node.x); ToCond(node.y); node.mode := mCond;
				IF sym = S.and THEN
					node.fLink := merged(node.x.fLink, node.y.fLink);
					node.tLink := node.y.tLink
				ELSE
					node.tLink := merged(node.x.tLink, node.y.tLink);
					node.fLink := node.y.fLink
				END
			ELSIF (sym >= S.eql) & (sym <= S.geq) THEN
				IF (node.y.type = B.strType) & (node.x.type = B.charType)
				THEN LoadVar(node.x); StrToChar(node.y)
				ELSIF (node.x.type = B.strType) & (node.y.type = B.charType)
				THEN LoadVar(node.y); StrToChar(node.x)
				ELSIF ~(node.y.type.form IN {B.tArray, B.tStr}) THEN
					LoadVar(node.x); LoadVar(node.y)
				ELSE RefToRegI(node.x); RefToRegI(node.y)
				END;
				node.mode := mCond; node.fLink := NIL; node.tLink := NIL
			ELSIF sym = S.is THEN
				LoadTypeTag(node.x); LoadTypeDesc(node.y);
				node.mode := mCond; node.fLink := NIL; node.tLink := NIL
			ELSIF sym = S.in THEN
				LoadVar(node.x); LoadVar(node.y);
				node.mode := mCond; node.fLink := NIL; node.tLink := NIL
			ELSIF sym = S.arrow THEN
				LoadVar(node.x); node.mode := mRegI
			ELSIF sym = S.period THEN
				node.mode := node.x.mode; node.ref := node.x.ref
			ELSIF sym = S.not THEN
				ToCond(node.x); node.mode := mCond;
				node.fLink := node.x.tLink; node.tLink := node.x.fLink
			ELSIF sym = S.lparen THEN
				LoadTypeDesc(node.y);
				node.mode := node.x.mode; node.ref := node.x.ref
			ELSIF sym = S.lbrak THEN
				IF node.y.mode = mImm THEN
					node.mode := node.x.mode; node.ref := node.x.ref
				ELSE LoadVar(node.y); node.mode := mRegI
				END
			ELSIF sym = S.par THEN
				IF 
			END
		ELSE ASSERT(FALSE)
		END
	END;
	RETURN node
END MakeNode;

PROCEDURE Pass1(node: Node; usedRegs: SET; );
BEGIN
	IF node.op = S.par THEN
		IF node.x & 
	END
END Pass1;

PROCEDURE Generate*(modinit: B.Node);
BEGIN
	AllocStaticData;
	
END Generate;

PROCEDURE Init*(modid: B.IdStr);
BEGIN
	varSize := 0; staticSize := 64;
	impList := NIL; procList := NIL; curProc := NIL;
	B.intType.size := 8; B.intType.align := 8;
	B.byteType.size := 1; B.byteType.align := 1;
	B.charType.size := 2; B.charType.align := 2;
	B.boolType.size := 1; B.boolType.align := 1;
	B.setType.size := 8; B.setType.align := 8;
	B.realType.size := 8; B.realType.align := 8;
	B.nilType.size := 8; B.nilType.align := 8
END Init;

END Generator1.
