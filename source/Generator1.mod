MODULE Generator1;

IMPORT
	SYSTEM, Sys := BaseSys,
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
	NOT = 2F6H; NEG = 3F6H; IDIVa = 7F7H; INC_ = 0FEH; DEC_ = 1FEH;
	CALL = 2FFH; JMP = 4FFH; PUSH = 6FFH;
	LDMXCSR = 2AE0FH; STMXCSR = 3AE0FH;
	
	(* Opcodes used with EmitRmImm *)
	ADDi = 80H; ORi = 180H; ANDi = 480H; SUBi = 580H; XORi = 680H; CMPi = 780H;
	RORi = 1C0H; SHLi = 4C0H; SHRi = 5C0H; SARi = 7C0H; MOVi = 0C6H;
	TESTi = 76H; BTi = 4BA0FH; BTSi = 5BA0FH; BTRi = 6BA0FH; BTCi = 7BA0FH;
	IMULi = 69H (* Special case *);
	
	(* Opcodes used with EmitBare *)
	CQO = 9948H; LEAVE = 0C9H; RET = 0C3H; INT3 = 0CCH;
	CMPSB = 0A6H; CMPSW = 0A766H; CMPSD = 0A7H;
	
	(* REP instructions *)
	MOVSrep = 0A4H;
	
	(* Opcodes used with EmitXmmRm *)
	SseMOVD = 6E0F66H; SseMOVDd = 7E0F66H;
	MOVSS = 100FF3H; MOVSSd = 110FF3H; MOVSD = 100FF2H; MOVSDd = 110FF2H;
	ADDSD = 580FF2H; MULSD = 590FF2H; SUBSD = 5C0FF2H; DIVSD = 5E0FF2H;
	ADDSS = 580FF3H; MULSS = 590FF3H; SUBSS = 5C0FF3H; DIVSS = 5E0FF3H;
	ADDPS = 580F00H; MULPS = 590F00H; SUBPS = 5C0F00H; DIVPS = 5E0F00H;
	ANDPS = 540F00H; ANDNPS = 550F00H; ORPS = 560F00H; XORPS = 570F00H;
	ANDPD = 540F66H; ANDNPD = 550F66H; ORPD = 560F66H; XORPD = 570F66H;
	MOVAPS = 280F00H; MOVAPSd = 290F00H; COMISS = 2F0F00H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	
	(* Item mode *)
	mReg = 0; mXReg = 1; mImm = 2; mRegI = 3; mIP = 4; mSP = 5; mBP = 6;
	mCond = 7; mProc = 8; mType = 9; mBX = 10;
	
	(* Trap code *)
	divideTrap = 0;
	arrayTrap = 1;
	typeTrap = 2;
	assertTrap = 3;
	nilTrap = 4;
	modkeyTrap = 5;
	overflowTrap = 6;
	stringTrap = 7;
	
TYPE
	Block = POINTER TO RECORD
		code: Sys.MemFile; rCode: Sys.MemFileRider;
		jc: BYTE; call: BOOLEAN; next, link, jDst: Block
	END;
	
	Proc = POINTER TO RECORD
		obj: B.Proc; blk: Block; next: Proc
	END;
	
	Item = RECORD
		mode, op, r, rm: BYTE; ref, par: BOOLEAN;
		a, b, c, strlen: INTEGER; aLink, bLink: Block; type: B.Type
	END;
	
	MakeItemState = RECORD
		avoid, xAvoid: SET; bestReg, bestXReg: BYTE
	END;
	
VAR
	(* forward decl *)
	MakeItem0: PROCEDURE(VAR x: Item; obj: B.Object);

	firstBlk, curBlk: Block;
	procList, curProc: Proc;

	mem: RECORD
		mod, rm, bas, idx, scl, disp: INTEGER
	END;
	allocReg, allocXReg: SET;
	MkItmStat: MakeItemState; (* State for MakeItem procedures in Pass 2 *)
	
	varSize, staticSize: INTEGER;
	impList: B.Ident;
	
	parPassingReg: ARRAY 4 OF INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE log2(n: INTEGER): INTEGER;
	VAR e: INTEGER;
BEGIN e := 0;
	IF n > 1 THEN
		WHILE n > 1 DO
			IF ODD(n) THEN e := -1; n := 0 ELSE INC (e); n := n DIV 2 END 
		END
	ELSIF n # 1 THEN e := -1
	END;
	RETURN e
END log2;

PROCEDURE IntToSet(n: INTEGER): SET;
	RETURN SYSTEM.VAL(SET, n)
END IntToSet;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewBlock(VAR blk: Block);
BEGIN NEW(blk); Sys.NewMemFile(blk.code);
	Sys.SetMemFile(blk.rCode, blk.code, 0);
END NewBlock;

PROCEDURE OpenBlock(oldBlkCond: INTEGER);
	VAR newBlk: Block;
BEGIN NewBlock(newBlk); newBlk.link := NIL; newBlk.jDst := NIL;
	curBlk.jc := oldBlkCond; curBlk.next := newBlk; curBlk := newBlk
END OpenBlock;

PROCEDURE Put1(n: INTEGER);
BEGIN Sys.WriteMemFile(curBlk.rCode, n)
END Put1;

PROCEDURE Put2(n: INTEGER);
BEGIN Put1(n); n := n DIV 256; Put1(n)
END Put2;

PROCEDURE Put4(n: INTEGER);
BEGIN Put2(n); n := n DIV 10000H; Put2(n)
END Put4;

PROCEDURE Put8(n: INTEGER);
BEGIN Put4(n); n := n DIV 100000000H; Put4(n)
END Put8;

PROCEDURE CodeLen(): INTEGER;
	RETURN Sys.MemFileLength(curBlk.code)
END CodeLen;

PROCEDURE CodeLen0(blk: Block): INTEGER;
	RETURN Sys.MemFileLength(blk.code)
END CodeLen0;

PROCEDURE MergeNextBlock(blk: Block);
	VAR next: Block;
BEGIN ASSERT(blk.jDst = NIL);
	next := blk; Sys.MergeMemFile(blk.code, next.code); blk.next := next.next;
	Sys.SetMemFile(blk.rCode, blk.code, CodeLen0(blk));
	IF blk.next = NIL THEN curBlk := blk END
END MergeNextBlock;

PROCEDURE MergeFrom(blk: Block);
BEGIN WHILE blk # curBlk DO MergeNextBlock(blk) END
END MergeFrom;

PROCEDURE FindProcBlk(obj: B.Object): Block;
	RETURN NIL
END FindProcBlk;
	
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)
	
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
		OR (mem.mod = 2) THEN Put4(mem.disp)
		ELSIF mem.mod = 1 THEN Put1(mem.disp)
		END
	END
END EmitModRM;

(* -------------------------------------------------------------------------- *)
	
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
		IF (disp = 0) & ~(reg IN {reg_BP, reg_R13})
		THEN mem.mod := 0 ELSE mem.mod := 1
		END
	ELSE mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		mem.bas := reg_SP; mem.idx := reg_SP; mem.scl := 0
	END
END SetRm_regI;

PROCEDURE SetRmOperand(x: Item);
BEGIN
	IF x.mode = mSP THEN
		mem.rm := reg_SP; mem.bas := reg_SP; mem.idx := reg_SP;
		mem.scl := 0; mem.disp := x.a;
		IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
	ELSIF x.mode = mBP THEN
		mem.rm := reg_BP; mem.disp := x.a;
		IF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1 ELSE mem.mod := 2 END
	ELSIF x.mode = mIP THEN mem.rm := reg_BP; mem.disp := x.a; mem.mod := 0
	ELSIF x.mode = mBX THEN mem.rm := reg_B; mem.disp := x.a;
		IF x.a = 0 THEN mem.mod := 0 
		ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
		ELSE mem.mod := 2
		END
	ELSIF x.mode = mRegI THEN SetRm_regI(x.r, x.a)
	ELSIF x.mode = mReg THEN SetRm_reg(x.r)
	ELSIF x.mode = mProc THEN mem.rm := reg_B; mem.disp := x.a;
		IF x.a = 0 THEN mem.mod := 0 
		ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
		ELSE mem.mod := 2
		END
	END
END SetRmOperand;

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
	SetRm_reg (rm); EmitREX(0, 4); Put1(50H + rm MOD 8)
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN
	SetRm_reg (rm); EmitREX(0, 4); Put1(58H + rm MOD 8)
END PopR;

PROCEDURE Branch(disp: INTEGER);
BEGIN Put1(0E9H); Put4(disp)
END Branch;

PROCEDURE Branch1(disp: INTEGER);
BEGIN Put1(0EBH); Put1(disp)
END Branch1;

PROCEDURE CallNear(disp: INTEGER);
BEGIN Put1(0E8H); Put4(disp)
END CallNear;

PROCEDURE CondBranch(cond, disp: INTEGER);
BEGIN Put1(0FH); Put1(80H + cond); Put4(disp)
END CondBranch;

PROCEDURE CondBranch1(cond, disp: INTEGER);
BEGIN Put1(70H + cond); Put1(disp)
END CondBranch1;

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
(* Pass 1 *)

PROCEDURE Pass1(obj: B.Object);
	VAR node: B.Node; fpar: B.Ident;
	
	PROCEDURE ParNode(par: B.Node; fpar: B.Ident; n: INTEGER);
		VAR i: INTEGER; varpar, openArr: BOOLEAN; ftype: B.Type;
	BEGIN Pass1(par.left);
		par.regUsed := par.left.regUsed; par.xRegUsed := par.left.xRegUsed;
		ftype := fpar.obj.type; openArr := B.IsOpenArray(ftype);
		varpar := fpar.obj(B.Par).varpar; 
		IF openArr OR varpar & (ftype.form = B.tRec)
		THEN i := 2 ELSE i := 1
		END;
		WHILE i > 0 DO 
			IF (ftype.form # B.tReal) OR varpar THEN
				IF n = 0 THEN INCL(par.regUsed, reg_C)
				ELSIF n = 1 THEN INCL(par.regUsed, reg_D)
				ELSIF n = 2 THEN INCL(par.regUsed, reg_R8)
				ELSIF n = 3 THEN INCL(par.regUsed, reg_R9)
				END
			ELSE INCL(par.xRegUsed, n)
			END;
			DEC(i); INC(n)
		END;
		IF par.right # NIL THEN ParNode(par.right(B.Node), fpar.next, n);
			par.regUsed := par.regUsed + par.right.regUsed;
			par.xRegUsed := par.xRegUsed + par.right.xRegUsed
		END
	END ParNode;
	
BEGIN (* Pass1 *)
	obj.regUsed := {}; obj.xRegUsed := {};
	IF obj IS B.Node THEN node := obj(B.Node);
		IF node.op # S.call THEN Pass1(node.left);
			node.regUsed := node.left.regUsed;
			node.xRegUsed := node.left.xRegUsed;
			IF node.right # NIL THEN Pass1(node.right);
				node.regUsed := node.regUsed + node.right.regUsed;
				node.xRegUsed := node.xRegUsed + node.right.xRegUsed
			END;
			IF (node.op = S.div) OR (node.op = S.mod) THEN
				node.regUsed := node.regUsed + {reg_A, reg_D}
			ELSIF (node.op = S.sproc) & (node.left(B.SProc).id IN B.sfShifts)
				& ~(node.right IS B.Const) THEN
				INCL(node.regUsed, reg_C); INCL(node.right.regUsed, reg_C)
			ELSIF (node.op >= S.eql) & (node.op <= S.geq)
				& B.IsStr(node.left.type) THEN
				node.regUsed := node.regUsed + {reg_SI, reg_DI};
				INCL(node.right.regUsed, reg_DI)
			ELSIF node.op = S.upto THEN INCL(node.regUsed, reg_C)
			END
		ELSE Pass1(node.left);
			node.regUsed :=
				{reg_A, reg_C, reg_D, reg_R8, reg_R9, reg_R10, reg_R11};
			node.xRegUsed := {0..5};
			node.regUsed := node.regUsed + node.left.regUsed;
			node.xRegUsed := node.xRegUsed + node.left.xRegUsed;
			fpar := node.left.type.fields;
			IF fpar # NIL THEN ParNode(node.right(B.Node), fpar, 0);
				node.regUsed := node.regUsed + node.right.regUsed;
				node.xRegUsed := node.xRegUsed + node.right.xRegUsed
			END
		END
	END
END Pass1;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Pass 2 *)

PROCEDURE negated(cond: INTEGER): INTEGER;
BEGIN IF ODD(cond) THEN DEC(cond) ELSE INC(cond) END; RETURN cond
END negated;

PROCEDURE FJump0(src: Block);
	VAR off, cc: INTEGER; blk: Block;
BEGIN off := 0; blk := src.next; cc := src.jc;
	WHILE blk # src.jDst DO
		ASSERT(blk.jDst = NIL); INC(off, CodeLen0(blk)); blk := blk.next
	END;
	IF (off > 0) & (cc # ccNever) THEN
		blk := curBlk; curBlk := src; 
		IF cc = ccAlways THEN
			IF off <= 127 THEN Branch1(off) ELSE Branch(off) END
		ELSIF off <= 127 THEN CondBranch1(cc, off) ELSE CondBranch(cc, off)
		END;
		curBlk := blk
	END;
	src.jDst := NIL
END FJump0;

PROCEDURE FJump(src: Block);
	VAR off, cc: INTEGER; blk: Block;
BEGIN off := 0; blk := src.next; cc := src.jc;
	WHILE blk # src.jDst DO INC(off, CodeLen0(blk));
		IF (blk.jDst # NIL) & (blk.jc # ccNever) THEN
			IF blk.jc = ccAlways THEN INC(off, 5) ELSE INC(off, 6) END
		END;
		blk := blk.next
	END;
	IF (off > 0) & (cc # ccNever) THEN
		blk := curBlk; curBlk := src; 
		IF cc = ccAlways THEN
			IF off <= 127 THEN Branch1(off) ELSE Branch(off) END
		ELSIF off <= 127 THEN CondBranch1(cc, off) ELSE CondBranch(cc, off)
		END;
		curBlk := blk
	END;
	src.jDst := NIL
END FJump;

PROCEDURE BJump(src: Block);
	VAR off, cc: INTEGER; blk: Block;
BEGIN off := 0; blk := src.jDst; cc := src.jc;
	WHILE blk # src DO INC(off, CodeLen0(blk));
		IF (blk.jDst # NIL) & (blk.jc # ccNever) THEN
			IF blk.jc = ccAlways THEN INC(off, 5) ELSE INC(off, 6) END
		END;
		blk := blk.next
	END;
	INC(off, CodeLen0(src));
	IF off + 2 <= 128 THEN INC(off, 2)
	ELSIF cc = ccAlways THEN INC(off, 5) ELSE INC(off, 6)
	END;
	IF cc # ccNever THEN
		blk := curBlk; curBlk := src; 
		IF cc = ccAlways THEN
			IF off <= 128 THEN Branch1(-off) ELSE Branch(-off) END
		ELSIF off <= 128 THEN CondBranch1(cc, -off) ELSE CondBranch(cc, -off)
		END;
		curBlk := blk
	END;
	src.jDst := NIL
END BJump;

PROCEDURE FixLinkWith(L, dst: Block);
	VAR L1: Block;
BEGIN
	WHILE L # NIL DO
		L.jDst := dst; L1 := L.link; L.link := NIL; L := L1
	END
END FixLinkWith;

PROCEDURE FixLink(L: Block);
BEGIN FixLinkWith(L, curBlk)
END FixLink;

PROCEDURE merged(L0, L1: Block): Block;
	VAR L2, L3: Block;
BEGIN 
	IF L0 # NIL THEN L3 := L0;
		REPEAT L2 := L3; L3 := L3.link UNTIL L3 = NIL;
		L2.link := L1; L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE SetCond(VAR x: Item; c: INTEGER);
BEGIN x.mode := mCond; x.aLink := NIL; x.bLink := NIL; x.c := c
END SetCond;

PROCEDURE OpToCc(op: INTEGER): INTEGER;
BEGIN
	IF op = S.eql THEN op := ccZ
	ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccB
	ELSIF op = S.gtr THEN op := ccA
	ELSIF op = S.leq THEN op := ccBE
	ELSE op := ccAE
	END;
	RETURN op
END OpToCc;

PROCEDURE IntOpToCc(op: INTEGER): INTEGER;
BEGIN
	IF op = S.eql THEN op := ccZ
	ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccL
	ELSIF op = S.gtr THEN op := ccG
	ELSIF op = S.leq THEN op := ccLE
	ELSE op := ccGE
	END;
	RETURN op
END IntOpToCc;

PROCEDURE Trap(cond, trapno: INTEGER);
	VAR blk1, blk2: Block;
BEGIN
	IF ~(cond IN {ccAlways, ccNever}) THEN
		blk1 := curBlk; OpenBlock(cond);
		blk2 := curBlk; OpenBlock(ccAlways); blk1.jDst := curBlk;
		MoveRI(reg_A, 1, trapno); MoveRI(reg_C, 4, S.Pos()); EmitBare(INT3);
		OpenBlock(0); blk2.jDst := curBlk; FJump0(blk2); FJump0(blk1);
		MergeFrom(blk1)
	ELSIF cond = ccAlways THEN
		MoveRI(reg_A, 1, trapno); MoveRI(reg_C, 4, S.Pos()); EmitBare(INT3)
	END
END Trap;

(* -------------------------------------------------------------------------- *)

PROCEDURE ResetMkItmStat;
BEGIN
	MkItmStat.avoid := {}; MkItmStat.bestReg := 255;
	MkItmStat.xAvoid := {}; MkItmStat.bestXReg := 255
END ResetMkItmStat;

PROCEDURE AllocReg(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.avoid + allocReg + {reg_SP, reg_BP, reg_B};
	IF (MkItmStat.bestReg = 255) OR (MkItmStat.bestReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END
	ELSE reg := MkItmStat.bestReg
	END;
	INCL(allocReg, reg);
	RETURN reg
END AllocReg;

PROCEDURE AllocReg2(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := allocReg + {reg_SP, reg_BP, reg_B};
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	RETURN reg
END AllocReg2;

PROCEDURE AllocReg3(avoid: SET): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := allocReg + {reg_SP, reg_BP, reg_B};
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	INCL(allocReg, reg);
	RETURN reg
END AllocReg3;

PROCEDURE SetAlloc(reg: BYTE);
BEGIN INCL(allocReg, reg)
END SetAlloc;

PROCEDURE SetAllocX(reg: BYTE);
BEGIN INCL(allocXReg, reg)
END SetAllocX;

PROCEDURE SetBestReg(reg: BYTE);
BEGIN MkItmStat.bestReg := reg
END SetBestReg;

PROCEDURE AllocXReg(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.xAvoid + allocXReg;
	IF (MkItmStat.bestXReg = -1) OR (MkItmStat.bestXReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END
	ELSE reg := MkItmStat.bestXReg
	END;
	INCL(allocXReg, reg);
	RETURN reg
END AllocXReg;

PROCEDURE AllocXReg2(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := allocXReg;
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	RETURN reg
END AllocXReg2;

PROCEDURE FreeReg(reg: BYTE);
BEGIN EXCL(allocReg, reg)
END FreeReg;

PROCEDURE FreeXReg(reg: BYTE);
BEGIN EXCL(allocXReg, reg)
END FreeXReg;

(* -------------------------------------------------------------------------- *)

PROCEDURE RefToRegI(VAR x: Item);
	VAR reg: BYTE;
BEGIN
	IF x.ref & (x.mode # mProc) THEN
		ASSERT(x.mode IN {mReg, mRegI, mSP, mIP, mBP});
		IF x.mode IN {mReg, mRegI} THEN reg := x.r ELSE reg := AllocReg() END;
		SetRmOperand(x); EmitRegRm(MOVd, reg, 8);
		x.mode := mRegI; x.a := x.b; x.ref := FALSE
	END
END RefToRegI;

PROCEDURE Load(VAR x: Item);
	VAR r, r2: BYTE; size: INTEGER; oldType: B.Type;
BEGIN RefToRegI(x);
	IF x.type.form # B.tReal THEN
		IF x.mode # mReg THEN size := x.type.size;
			IF x.mode # mRegI THEN r := AllocReg() ELSE r := x.r END;
			IF x.mode = mImm THEN MoveRI(r, 8, x.a)
			ELSIF x.mode IN {mRegI, mSP, mIP, mBP} THEN SetRmOperand(x);
				IF size >= 4 THEN EmitRegRm(MOVd, r, size)
				ELSE EmitMOVZX(r, size)
				END
			ELSIF x.mode = mProc THEN SetRmOperand(x);
				IF ~x.ref THEN EmitRegRm(LEA, r, 4)
				ELSE EmitRegRm(MOVd, r, 8)
				END
			ELSE ASSERT(FALSE)
			END;
			x.mode := mReg; x.r := r
		END
	ELSIF x.mode # mXReg THEN
		IF x.mode = mImm THEN r2 := AllocReg2();
			MoveRI(r2, x.type.size, x.a); r := AllocXReg(); SetRm_reg(r2)
		ELSE r := AllocXReg(); SetRmOperand(x)
		END;
		EmitXmmRm(SseMOVD, r, x.type.size);
		IF x.mode IN {mReg, mRegI} THEN FreeReg(x.r) END;
		x.mode := mXReg; x.r := r
	END
END Load;

PROCEDURE LoadAdr(VAR x: Item);
	VAR r: BYTE;
BEGIN RefToRegI(x); SetRmOperand(x);
	IF x.mode = mRegI THEN r := x.r ELSE r := AllocReg() END;
	IF (x.mode # mRegI) OR (x.a # 0) THEN EmitRegRm(LEA, r, 8) END;
	x.r := r; x.mode := mReg
END LoadAdr;

PROCEDURE ArrayLen(VAR len: Item; x: Item);
BEGIN len.type := B.intType; len.ref := FALSE;
	IF x.type = B.strType THEN len.mode := mImm; len.a := x.strlen
	ELSIF B.IsOpenArray(x.type) THEN len.mode := mBP; len.a := x.c
	ELSE len.mode := mImm; len.a := x.type.len
	END
END ArrayLen;

PROCEDURE TypeTag(VAR x: Item);
BEGIN
	IF x.type.form = B.tPtr THEN Load(x); x.mode := mRegI; x.a := -8
	ELSIF x.mode = mBP THEN x.a := x.a + 8; x.ref := FALSE
	ELSE ASSERT(FALSE)
	END;
	x.type := B.intType
END TypeTag;

PROCEDURE TypeTag2(VAR tag: Item; x: Item);
BEGIN tag := x;
	IF (x.type.form = B.tPtr) & (x.mode IN {mReg, mRegI}) THEN
		tag.r := AllocReg(); SetRmOperand(x); EmitRegRm(MOVd, tag.r, 8);
		tag.mode := mReg
	END; TypeTag(tag)
END TypeTag2;

PROCEDURE TypeDesc(VAR x: Item; tp: B.Type);
BEGIN x.mode := mBX; x.a := tp.adr; x.type := B.intType; x.ref := tp.lev < 0
END TypeDesc;

PROCEDURE RelocReg(VAR reg: BYTE; newReg: BYTE);
BEGIN EmitRR(MOVd, newReg, 8, reg); FreeReg(reg); reg := newReg
END RelocReg;

PROCEDURE RelocXReg(VAR reg: BYTE; newReg: BYTE);
BEGIN SetRm_reg(reg); EmitXmmRm(MOVSD, newReg, 8); FreeXReg(reg); reg := newReg
END RelocXReg;

PROCEDURE AvoidUsedBy(obj: B.Object);
BEGIN
	IF obj # NIL THEN
		IF (obj.regUsed # {}) OR (obj.xRegUsed # {}) THEN
			MkItmStat.avoid := MkItmStat.avoid + obj.regUsed;
			MkItmStat.xAvoid := MkItmStat.xAvoid + obj.xRegUsed
		END
	END
END AvoidUsedBy;

PROCEDURE RelocVolatile(VAR x: Item);
BEGIN
	IF (x.mode = mReg) & (x.r IN MkItmStat.avoid) THEN
		RelocReg(x.r, AllocReg())
	ELSIF (x.mode = mXReg) & (x.r IN MkItmStat.xAvoid) THEN
		RelocReg(x.r, AllocXReg())
	ELSE ASSERT(FALSE)
	END
END RelocVolatile;

PROCEDURE LoadRight(VAR y: Item; node: B.Node);
	VAR oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; ResetMkItmStat;
	MakeItem0(y, node.right); Load(y); MkItmStat := oldStat
END LoadRight;

PROCEDURE LoadLeftRight(VAR x, y: Item; node: B.Node);
	VAR oldStat: MakeItemState;
BEGIN
	oldStat := MkItmStat; AvoidUsedBy(node.right);
	MakeItem0(x, node.left); Load(x); RelocVolatile(x);
	ResetMkItmStat; MakeItem0(y, node.right); Load(y); MkItmStat := oldStat
END LoadLeftRight;

PROCEDURE LoadLeftRight2(VAR x, y: Item; node: B.Node);
BEGIN
	AvoidUsedBy(node.right); MakeItem0(x, node.left); Load(x);
	RelocVolatile(x); ResetMkItmStat; MakeItem0(y, node.right); Load(y)
END LoadLeftRight2;

PROCEDURE Add(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tInt THEN EmitRR(ADDd, x.r, 8, y.r)
	ELSIF node.type.form = B.tSet THEN EmitRR(ORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(ADDSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
END Add;

PROCEDURE Subtract(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN 
	IF node.right # NIL THEN LoadLeftRight(x, y, node);
		IF node.type.form = B.tInt THEN EmitRR(SUBd, x.r, 8, y.r)
		ELSIF node.type.form = B.tSet THEN
			EmitR(NOT, y.r, 8); EmitRR(ANDd, x.r, 8, y.r)
		ELSIF node.type = B.realType THEN
			SetRm_reg(y.r); EmitXmmRm(SUBSD, x.r, 4)
		ELSE ASSERT(FALSE)
		END;
		IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
	ELSE MakeItem0(x, node.left); Load(x);
		IF node.type.form = B.tInt THEN EmitR(NEG, x.r, 8)
		ELSIF node.type.form = B.tSet THEN EmitR(NOT, x.r, 8)
		ELSIF node.type = B.realType THEN
			y.r := AllocXReg2(); SetRm_reg(y.r); EmitXmmRm(XORPD, y.r, 4);
			SetRm_reg(x.r); EmitXmmRm(SUBSD, y.r, 4);
			SetRm_reg(y.r); EmitXmmRm(MOVSD, x.r, 4)
		END
	END
END Subtract;

PROCEDURE Multiply(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tInt THEN EmitRR(IMUL, x.r, 8, y.r)
	ELSIF node.type.form = B.tSet THEN EmitRR(ANDd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(MULSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
END Multiply;

PROCEDURE IntDiv(VAR x: Item; node: B.Node);
	CONST divReg = {reg_A, reg_D};
	VAR y: Item; blk1, blk2: Block;
		oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; ResetMkItmStat;
	SetBestReg(reg_A); AvoidUsedBy(node.right);
	MakeItem0(x, node.left); Load(x); RelocVolatile(x);
	ResetMkItmStat; MkItmStat.avoid := MkItmStat.avoid + divReg;
	MakeItem0(y, node.right); Load(y); RelocVolatile(y);
	IF x.r # reg_A THEN RelocReg(x.r, reg_A); SetAlloc(reg_A) END;
	
	EmitBare(CQO); EmitR(IDIVa, y.r, 8); EmitRR(TEST, reg_D, 8, reg_D);
	blk1 := curBlk; OpenBlock(ccL);
	blk2 := curBlk; OpenBlock(ccAlways); blk1.jDst := curBlk;
	IF node.op = S.div THEN EmitRI(SUBi, reg_A, 8, 1)
	ELSE EmitRR(ADDd, reg_D, 8, y.r)
	END;
	OpenBlock(0); blk2.jDst := curBlk;
	FJump0(blk2); FJump0(blk1); MergeFrom(blk1);
	
	IF node.op = S.mod THEN FreeReg(reg_A); x.r := reg_D; SetAlloc(reg_D) END;
	FreeReg(y.r); MkItmStat := oldStat
END IntDiv;

PROCEDURE Divide(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tSet THEN EmitRR(XORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(DIVSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
END Divide;

PROCEDURE LoadCond(VAR x: Item; obj: B.Object);
	VAR oldStat: MakeItemState;
BEGIN MakeItem0(x, obj);
	IF x.mode # mCond THEN oldStat := MkItmStat; ResetMkItmStat;
		IF x.mode = mImm THEN SetCond(x, ccNever - x.a)
		ELSE Load(x); EmitRR(TEST, x.r, 4, x.r); FreeReg(x.r); SetCond(x, ccNZ)
		END;
		MkItmStat := oldStat
	END
END LoadCond;

PROCEDURE And(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadCond(x, node.left); x.aLink := curBlk;
	OpenBlock(negated(x.c)); FixLink(x.bLink);
	LoadCond(y, node.right); x.aLink := merged(y.aLink, x.aLink);
	x.bLink := y.bLink; x.c := y.c
END And;

PROCEDURE Or(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadCond(x, node.left); x.bLink := curBlk;
	OpenBlock(x.c); FixLink(x.aLink);
	LoadCond(y, node.right); x.bLink := merged(y.bLink, x.bLink);
	x.aLink := y.aLink; x.c := y.c
END Or;

PROCEDURE Not(VAR x: Item; node: B.Node);
	VAR t: Block;
BEGIN LoadCond(x, node.left); x.c := negated(x.c);
	t := x.aLink; x.aLink := x.bLink; x.bLink := t
END Not;

PROCEDURE Compare(VAR x: Item; node: B.Node);
	VAR cond: INTEGER; cx, r: BYTE; oldStat: MakeItemState;
		y, len: Item; orgBlk, blk2, blk3: Block;
BEGIN oldStat := MkItmStat; ResetMkItmStat;
	IF node.left.type.form IN B.typScalar - {B.tReal} THEN
		LoadLeftRight2(x, y, node); EmitRR(CMPd, x.r, 8, y.r);
		IF x.type.form = B.tInt THEN cond := IntOpToCc(node.op)
		ELSE cond := OpToCc(node.op)
		END;
		FreeReg(x.r); FreeReg(y.r); SetCond(x, cond)
	ELSIF B.IsStr(node.left.type) THEN
		SetBestReg(reg_SI); AvoidUsedBy(node.right);
		MakeItem0(x, node.left); LoadAdr(x); RelocVolatile(x);
		ResetMkItmStat; SetBestReg(reg_DI);
		MakeItem0(y, node.right); LoadAdr(y);
		IF y.r # reg_DI THEN RelocReg(y.r, reg_DI); SetAlloc(reg_DI) END;
		IF x.r # reg_SI THEN RelocReg(x.r, reg_SI); SetAlloc(reg_SI) END;
		
		cx := AllocReg3({}); EmitRR(XOR, cx, 4, cx); orgBlk := curBlk;
		OpenBlock(0); EmitRI(ADDi, cx, 8, 1);
		ArrayLen(len, x); Load(len); EmitRR(CMPd, cx, 8, len.r);
		Trap(ccA, stringTrap); FreeReg(len.r);
		ArrayLen(len, y); Load(len); EmitRR(CMPd, cx, 8, len.r);
		Trap(ccA, stringTrap); FreeReg(len.r); FreeReg(cx);
		
		EmitBare(CMPSW); blk2 := curBlk; OpenBlock(ccNZ);
		SetRm_regI(reg_SI, -2); EmitRmImm(CMPi, 2, 0);
		blk3 := curBlk; OpenBlock(ccNZ); blk2.jDst := curBlk;
		blk3.jDst := blk2; FJump(blk2); BJump(blk3); MergeFrom(orgBlk);
		FreeReg(reg_DI); FreeReg(reg_SI); SetCond(x, cond)
	END;
	MkItmStat := oldStat
END Compare;

PROCEDURE MemberTest(VAR x: Item; node: B.Node);
	VAR y: Item; oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; ResetMkItmStat;
	LoadLeftRight2(x, y, node); EmitRR(BT, y.r, 8, x.r);
	FreeReg(x.r); FreeReg(y.r); SetCond(x, ccC); MkItmStat := oldStat
END MemberTest;

PROCEDURE TypeTest(VAR x: Item; node: B.Node);
	VAR y, tag: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN oldStat := MkItmStat; ResetMkItmStat; tp := node.right.type;
	MakeItem0(x, node.left); TypeTag(x); Load(x); TypeDesc(y, tp); LoadAdr(y);
	SetRm_regI(x.r, tp.len * 8); EmitRegRm(CMP, y.r, 8);
	FreeReg(x.r); FreeReg(y.r); SetCond(x, ccZ); MkItmStat := oldStat
END TypeTest;

PROCEDURE Deref(VAR x: Item; node: B.Node);
BEGIN MakeItem0(x, node.left); Load(x); x.mode := mRegI; x.a := 0
END Deref;

PROCEDURE Field(VAR x: Item; node: B.Node);
	VAR off: INTEGER;
BEGIN MakeItem0(x, node.left); off := node.right(B.Field).off;
	IF x.ref THEN INC(x.b, off) ELSE INC(x.a, off) END
END Field;

PROCEDURE TypeCheck(VAR x: Item; node: B.Node);
	VAR tag, y: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN MakeItem0(x, node.left); oldStat := MkItmStat; ResetMkItmStat;
	tp := node.right.type; TypeDesc(y, tp); LoadAdr(y); TypeTag2(tag, x);
	Load(tag); SetRm_regI(tag.r, tp.len * 8); EmitRegRm(CMP, y.r, 8);
	FreeReg(tag.r); FreeReg(y.r); Trap(ccNZ, typeTrap); MkItmStat := oldStat
END TypeCheck;

PROCEDURE Index(VAR x: Item; node: B.Node);
	VAR idx, size, align, e: INTEGER; len, y: Item; bType: B.Type;
BEGIN MakeItem0(x, node.left); bType := x.type.base; align := bType.align;
	size := (bType.size + align - 1) DIV align * align;
	IF node.right IS B.Const THEN idx := node.right(B.Const).val;
		IF B.IsOpenArray(x.type) THEN ArrayLen(len, x); SetRmOperand(len);
			EmitRmImm(CMPi, 8, idx); Trap(ccBE, arrayTrap)
		END;
		IF x.ref THEN INC(x.b, idx*size) ELSE INC(x.a, idx*size) END
	ELSIF size > 0 THEN
		RefToRegI(x); LoadRight(y, node); ArrayLen(len, x);
		IF len.mode = mImm THEN EmitRI(CMPi, y.r, 8, len.a)
		ELSE SetRmOperand(len); EmitRegRm(CMPd, y.r, 8)
		END;
		Trap(ccAE, arrayTrap); e := log2(size);
		IF e > 0 THEN EmitRI(SHLi, y.r, 8, e)
		ELSIF e < 0 THEN EmitRI(IMULi, y.r, 8, size)
		END;
		EmitRR(ADDd, x.r, 8, y.r); FreeReg(y.r)
	END
END Index;

PROCEDURE SingletonSet(VAR x: Item; node: B.Node);
	VAR r: INTEGER; oldStat: MakeItemState;
BEGIN r := AllocReg(); oldStat := MkItmStat; ResetMkItmStat;
	MakeItem0(x, node.left); Load(x); EmitRR(XOR, r, 4, r);
	EmitRR(BTS, x.r, 8, r); FreeReg(x.r); x.r := r; MkItmStat := oldStat
END SingletonSet;

PROCEDURE RangeSet(VAR x: Item; node: B.Node);
	VAR r, r2: INTEGER; oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; INCL(MkItmStat.avoid, reg_C);
	r := AllocReg(); r2 := AllocReg3({reg_C}); ResetMkItmStat;
	MkItmStat.bestReg := reg_C; MakeItem0(x, node.left); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C); SetAlloc(reg_C) END;
	MoveRI(r, 8, -1); EmitR(SHLcl, r, 8); FreeReg(reg_C);
	MkItmStat.bestReg := reg_C; MakeItem0(x, node.left); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C); SetAlloc(reg_C) END;
	MoveRI(r2, 8, -2); EmitR(SHLcl, r2, 8); FreeReg(reg_C);
	EmitRR(XORd, r, 8, r2); FreeReg(r2); x.r := r; MkItmStat := oldStat
END RangeSet;

PROCEDURE ParReg(n: INTEGER): BYTE;
BEGIN
	IF n > 3 THEN n := 255
	ELSIF n = 0 THEN n := reg_C ELSIF n = 1 THEN n := reg_D
	ELSIF n = 2 THEN n := reg_R8 ELSIF n = 3 THEN n := reg_R9
	END;
	RETURN n
END ParReg;

PROCEDURE LoadParam(par: B.Node; n: INTEGER; ref: BOOLEAN);
	VAR x: Item;
BEGIN AvoidUsedBy(par.right);
	IF ~ref THEN
		IF par.left.type.form # B.tReal THEN SetBestReg(ParReg(n))
		ELSIF n < 4 THEN MkItmStat.bestXReg := n
		END;
		MakeItem0(x, par.left); Load(x)
	ELSE SetBestReg(ParReg(n)); MakeItem0(x, par.left); LoadAdr(x)
	END;
	IF n < 4 THEN RelocVolatile(x)
	ELSE SetRm_regI(reg_SP, n*8);
		IF x.mode = mReg THEN EmitRegRm(MOV, x.r, 8); FreeReg(x.r)
		ELSE EmitXmmRm(SseMOVDd, x.r, 8); FreeXReg(x.r)
		END
	END
END LoadParam;

PROCEDURE Parameter(par: B.Node; fpar: B.Ident; n: INTEGER);
	VAR varpar: BOOLEAN; ftype: B.Type; x, y: Item; i: INTEGER; r: BYTE;
BEGIN i := 1;
	ResetMkItmStat; ftype := fpar.obj.type; varpar := fpar.obj(B.Par).varpar;
	IF ftype.form = B.tArray THEN
		LoadParam(par, n, TRUE); IF B.IsOpenArray(ftype) THEN INC(i) END
	ELSIF (ftype.form = B.tRec) & varpar THEN
		IF varpar OR (ftype.size > 8) OR ~(ftype.size IN {0, 1, 2, 4, 8}) THEN
			LoadParam(par, n, TRUE); IF varpar THEN INC(i) END
		ELSE LoadParam(par, n, FALSE)
		END
	ELSE LoadParam(par, n, FALSE)
	END;
	IF par.right # NIL THEN
		Parameter(par.right(B.Node), fpar.next, n+i);
		IF n < 4 THEN
			IF x.mode = mReg THEN r := ParReg(n);
				IF x.r # r THEN RelocReg(x.r, r); SetAlloc(r) END
			ELSIF x.r # n THEN RelocXReg(x.r, n); SetAllocX(n)
			END
		END
	END;
	IF i = 2 THEN ResetMkItmStat; SetBestReg(ParReg(n+1));
		IF ftype.form = B.tArray THEN ArrayLen(y, x); Load(y);
		ELSIF (par.left IS B.Par) & par.left(B.Par).varpar THEN
			MakeItem0(y, par.left); TypeTag(y); Load(y)
		ELSE TypeDesc(y, x.type); LoadAdr(y)
		END;
		IF n > 2 THEN SetRm_regI(reg_SP, n*8+8);
			IF x.mode = mReg THEN EmitRegRm(MOV, x.r, 8); FreeReg(x.r)
			ELSE EmitXmmRm(SseMOVDd, x.r, 8); FreeXReg(x.r)
			END
		END
	END
END Parameter;

PROCEDURE Call(VAR x: Item; node: B.Node);
	VAR y: Item; oldStat: MakeItemState; oldAlloc, oldXAlloc: SET;
		blk: Block;
BEGIN oldStat := MkItmStat; ResetMkItmStat;
	oldAlloc := allocReg; oldXAlloc := allocXReg;
	IF node.left IS B.Proc THEN MakeItem0(x, node.left)
	ELSE AvoidUsedBy(node.right); MakeItem0(x, node.left);
		Load(x); RelocVolatile(x)
	END;
	IF node.right # NIL THEN
		Parameter(node.right(B.Node), node.left.type.fields, 0)
	END;
	IF x.mode = mReg THEN SetRm_reg(x.r); EmitRm(CALL, 4)
	ELSIF x.ref THEN SetRmOperand(x); EmitRm(CALL, 4)
	ELSE blk := curBlk; OpenBlock(ccAlways); blk.call := TRUE;
		blk.jDst := FindProcBlk(node.left)
	END;
	MkItmStat := oldStat; allocReg := oldAlloc; allocXReg := oldXAlloc;
	IF node.left.type.base # NIL THEN
		IF node.left.type.base.form # B.tReal THEN
			x.mode := mReg; x.r := reg_A; SetAlloc(reg_A)
		ELSE x.mode := mXReg; x.r := 0; SetAllocX(0)
		END
	END
END Call;

PROCEDURE SFunc(VAR x: Item; node: B.Node);
	VAR id: INTEGER; par1: B.Node; obj1, obj2: B.Object; blk: Block; r: BYTE;
		oldStat: MakeItemState; y: Item;
BEGIN id := node.left(B.SProc).id;
	par1 := node.right(B.Node); obj1 := par1.left;
	IF id = B.sfABS THEN MakeItem0(x, obj1); Load(x);
		IF x.type.form = B.tInt THEN EmitRR(TEST, x.r, 8, x.r);
			blk := curBlk; OpenBlock(ccGE); EmitR(NEG, x.r, 8);
			OpenBlock(0); blk.jDst := curBlk; FJump0(blk); MergeFrom(blk)
		ELSIF x.type = B.realType THEN r := AllocReg2(); SetRm_reg(r);
			EmitXmmRm(SeeMOVDd, x.r, 8); EmitRI(BTRi, r, 8, 63);
			SetRm_reg(r); EmitXmmRm(SseMOVD, x.r, 8)
		END
	ELSIF id = B.sfODD THEN
		oldStat := MkItmStat; ResetMkItmStat; MakeItem0(x, obj1); Load(x);
		EmitRI(ANDi, x.r, 4, 1); FreeReg(x.r); SetCond(x, ccNZ);
		MkItmStat := oldStat
	ELSIF id = B.sfLEN THEN (* x is open array *)
		MakeItem0(x, obj1); INC(x.a, 8); x.type := B.intType; Load(x)
	ELSIF id IN B.sfShifts THEN
		AvoidUsedBy(par1.right);
	END
END SFunc;

PROCEDURE MakeItem(VAR x: Item; obj: B.Object);
	VAR objv: B.Var; node: B.Node; size: INTEGER;
BEGIN x.type := obj.type; x.ref := FALSE; x.a := 0; x.b := 0; x.c := 0;
	IF obj IS B.Const THEN x.mode := mImm; x.a := obj(B.Const).val
	ELSIF obj IS B.Var THEN objv := obj(B.Var); x.a := objv.adr;
		IF objv.lev <= 0 THEN x.mode := mBX ELSE x.mode := mBP END;
		IF objv.lev < -1 THEN x.ref := TRUE END;
		IF objv IS B.Str THEN x.strlen := objv(B.Str).len
		ELSIF objv IS B.Par THEN x.par := TRUE;
			IF objv(B.Par).varpar OR (objv.type.form = B.tArray) THEN
				x.ref := TRUE
			ELSIF objv.type.form = B.tRec THEN
				IF (size <= 8) & (size IN {0, 1, 2, 4, 8}) THEN
					x.ref := TRUE
				END
			END;
			IF B.IsOpenArray(objv.type) THEN x.c := x.a + 8 END
		END
	ELSIF obj IS B.Proc THEN x.mode := mProc;
		IF obj(B.Proc).lev < -1 THEN x.ref := TRUE END
	ELSIF obj.class = B.cType THEN x.mode := mType
	ELSIF obj IS B.Node THEN node := obj(B.Node);
		IF node.op = S.plus THEN Add(x, node)
		ELSIF node.op = S.minus THEN Subtract(x, node)
		ELSIF node.op = S.times THEN Multiply(x, node)
		ELSIF (node.op = S.div) OR (node.op = S.mod) THEN IntDiv(x, node)
		ELSIF node.op = S.rdiv THEN Divide(x, node)
		ELSIF node.op = S.and THEN And(x, node)
		ELSIF node.op = S.or THEN Or(x, node)
		ELSIF node.op = S.not THEN Not(x, node)
		ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN Compare(x, node)
		ELSIF node.op = S.in THEN MemberTest(x, node)
		ELSIF node.op = S.is THEN TypeTest(x, node)
		ELSIF node.op = S.arrow THEN Deref(x, node)
		ELSIF node.op = S.period THEN Field(x, node)
		ELSIF node.op = S.lparen THEN TypeCheck(x, node)
		ELSIF node.op = S.lbrak THEN Index(x, node)
		ELSIF node.op = S.bitset THEN SingletonSet(x, node)
		ELSIF node.op = S.upto THEN RangeSet(x, node)
		ELSIF node.op = S.call THEN Call(x, node)
		ELSIF node.op = S.sproc THEN SFunc(x, node)
		END;
		x.type := node.type
	END;
END MakeItem;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Const folding during parsing phase *)
	
PROCEDURE CheckSetElement*(x: B.Object);
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val ELSE val := 0 END;
	IF (val < 0) OR (val > 63) THEN
		S.Mark('Set element must be >= 0 and <= 63')
	END
END CheckSetElement;

PROCEDURE ConstSingletonSet*(x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN
	IF x IS B.Const THEN val := x(B.Const).val MOD 64 ELSE val := 0 END;
	x := B.NewConst(B.setType, ORD({val}));
	RETURN x
END ConstSingletonSet;

PROCEDURE ConstRangeSet*(x, y: B.Object): B.Object;
	VAR beg, end: INTEGER;
BEGIN
	IF x IS B.Const THEN beg := x(B.Const).val MOD 64 ELSE beg := 0 END;
	IF y IS B.Const THEN end := y(B.Const).val MOD 64 ELSE end := 0 END;
	x := B.NewConst(B.setType, ORD({beg..end}));
	RETURN x
END ConstRangeSet;

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

PROCEDURE AbsConst*(x: B.Object): B.Object;
	VAR type: B.Type; val: INTEGER; val2: SET;
BEGIN type := x.type; val := x(B.Const).val;
	IF type = B.intType THEN
		IF val < 0 THEN val := -val END
	ELSIF type = B.byteType THEN type := B.intType
	ELSIF type = B.realType THEN
		val2 := SYSTEM.VAL(SET, val); EXCL(val2, 63); val := ORD(val2)
	END;
	x := B.NewConst(type, val)
	RETURN x
END AbsConst;

PROCEDURE OddConst*(x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN val := x(B.Const).val; x := B.NewConst(B.boolType, val MOD 2);
	RETURN x
END OddConst;

PROCEDURE ShiftConst*(fid: INTEGER; x, y: B.Object): B.Object;
	VAR xval, yval: INTEGER;
BEGIN xval := x(B.Const).val; yval := y(B.Const).val;
	IF fid = B.sfLSL THEN xval := LSL(xval, yval)
	ELSIF fid = B.sfASR THEN xval := ASR(xval, yval)
	ELSIF fid = B.sfROR THEN xval := ROR(xval, yval)
	END;
	x := B.NewConst(B.intType, xval);
	RETURN x
END ShiftConst;

PROCEDURE FloorConst*(x: B.Object): B.Object;
	VAR val, fraction, exp, p: INTEGER; sign: BOOLEAN;
BEGIN
	IF x.type = B.realType THEN
		val := x(B.Const).val; fraction := val MOD 10000000000000H;
		exp := val DIV 10000000000000H MOD 800H; sign := val < 0;
		IF exp = 0 (* subnormal *) THEN val := 0
		ELSIF exp = 7FFH (* Inf or NaN *) THEN S.Mark('Float too large')
		ELSE DEC(exp, 1023); INC(fraction, 10000000000000H); p := 52;
			IF exp < 0 THEN val := 0 ELSIF exp = 0 THEN val := 1
			ELSE WHILE (p > 0) & (exp > 0) DO DEC(p); DEC(exp) END;
				IF exp = 0 THEN val := ASR(fraction, p)
				ELSIF exp <= 11 THEN val := LSL(fraction, exp)
				ELSE S.Mark('Float too large')
				END
			END;
			IF sign THEN val := -val END
		END
	END;
	x := B.NewConst(B.intType, val);
	RETURN x
END FloorConst;

PROCEDURE FltConst*(x: B.Object): B.Object;
	CONST n52 = 10000000000000H;
	VAR val, exp, r: INTEGER; sign: BOOLEAN;
BEGIN val := x(B.Const).val;
	IF val = MinInt THEN val := -3C20000000000000H
	ELSE exp := 0; sign := val < 0; IF sign THEN val := -val END; r := 0;
		WHILE val < n52 DO val := val * 2; DEC(exp) END;
		WHILE val >= n52 * 2 DO
			INC(r, LSL(val MOD 2, exp)); val := val DIV 2; INC(exp)
		END;
		IF (exp > 0) & (r >= LSL(1, exp-1)) THEN INC(val);
			IF val >= n52 * 2 THEN val := val DIV 2; INC(exp) END
		END;
		INC(exp, 1023); val := val MOD n52 + exp * n52;
		IF sign THEN val := ORD(SYSTEM.VAL(SET, val) + {63}) END
	END;
	x := B.NewConst(B.realType, val);
	RETURN x
END FltConst;
	
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
				IF B.IsOpenArray(ftype)
				OR (ftype.form = B.tRec) & (ident.obj(B.Par).varpar)
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
		staticSize := staticSize + 8; B.modList[i].adr := -staticSize
	END
END AllocImportModules;

PROCEDURE AllocImport*(x: B.Object);
	VAR p: B.Ident;
BEGIN NEW(p); p.obj := x; p.next := impList; impList := p;
	staticSize := staticSize + 8;
	IF x IS B.Var THEN x(B.Var).adr := -staticSize
	ELSIF x IS B.Proc THEN x(B.Proc).adr := -staticSize
	ELSIF x.class = B.cType THEN x.type.adr := -staticSize 
	END
END AllocImport;

PROCEDURE AllocStaticData;
	VAR p: B.Ident; q: B.TypeList; x: B.Object;
BEGIN p := B.strList;
	WHILE p # NIL DO x := p.obj; staticSize := staticSize + 2*x(B.Str).len;
		x(B.Str).adr := -staticSize; p := p.next
	END;
	staticSize := staticSize + (-staticSize) MOD 8;
	q := B.recList;
	WHILE q # NIL DO
		staticSize := staticSize + 16 + 8 * (B.MaxExt + q.type.nptr);
		q.type.adr := -staticSize; q := q.next
	END
END AllocStaticData;

PROCEDURE FixGlobalVarAdr;
	VAR p: B.Ident; amount: INTEGER;
BEGIN p := B.universe.first; amount := staticSize + (-staticSize) MOD 4096;
	WHILE p # NIL DO
		IF p.obj IS B.Var THEN DEC(p.obj(B.Var).adr, amount) END;
		p := p.next
	END
END FixGlobalVarAdr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Generate*(modinit: B.Node);
BEGIN
	AllocStaticData; FixGlobalVarAdr;
	
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

BEGIN
	MakeItem0 := MakeItem;
	parPassingReg[0] := reg_C; parPassingReg[1] := reg_D;
	parPassingReg[2] := reg_R8; parPassingReg[3] := reg_R9
END Generator1.
