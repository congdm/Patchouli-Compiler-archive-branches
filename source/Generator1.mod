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
	SeeMOVD = 6E0F66H; SeeMOVDd = 7E0F66H;
	MOVSS = 100FF3H; MOVSSd = 110FF3H; MOVSD = 100FF2H; MOVSDd = 110FF2H;
	ADDSD = 580FF2H; MULSD = 590FF2H; SUBSD = 5C0FF2H; DIVSD = 5E0FF2H;
	ADDSS = 580FF3H; MULSS = 590FF3H; SUBSS = 5C0FF3H; DIVSS = 5E0FF3H;
	ADDPS = 580F00H; MULPS = 590F00H; SUBPS = 5C0F00H; DIVPS = 5E0F00H;
	ANDPS = 540F00H; ANDNPS = 550F00H; ORPS = 560F00H; XORPS = 570F00H;
	ANDPD = 540F66H; ANDNPD = 550F66H; ORPD = 560F66H; XORPD = 570F66H;
	MOVAPS = 280F00H; MOVAPSd = 290F00H; COMISS = 2F0F00H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	
	mReg = 0; mXReg = 1; mImm = 2; mRegI = 3; mIP = 4; mSP = 5; mBP = 6;
	mCond = 7; mProc = 8; mType = 9;
	
TYPE
	Proc = POINTER TO RECORD
		usedRegs: SET; adr, parWindowSize: INTEGER;
		prologue, epilogue, body, prologSize, epilogSize, bodySize: INTEGER;
		regSavingStackSize, regSavingStackPos: INTEGER;
		next: Proc
	END;
	
	Item = RECORD
		mode, op, r, rm: BYTE; ref, par: BOOLEAN;
		a, b, c, strlen: INTEGER; aLink, bLink: Block; type: B.Type
	END;
	
	Block = POINTER TO RECORD
		code: Sys.MemFile; rCode: Sys.MemFileRider;
		jc: INTEGER; next, link, jDst: Block
	END;
	
	MakeItemState = RECORD
		alloc, avoid, xAlloc, xAvoid: SET; bestReg, bestXReg: INTEGER
	END;
	
VAR
	(* forward decl *)
	MakeItem0: PROCEDURE(VAR x: Item; obj: B.Object);

	firstBlk, curBlk: Block;
	procList, curProc: Proc;

	mem: RECORD
		mod, rm, bas, idx, scl, disp: INTEGER
	END;
	MkItmStat: MakeItemState; (* State for MakeItem procedures in Pass 2 *)
	
	varSize, staticSize: INTEGER;
	impList: B.Ident;
	
	parPassingReg: ARRAY 4 OF INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewBlock(VAR blk: Block);
BEGIN NEW(blk); Sys.NewMemFile(blk.code); Sys.SetMemFile(blk.rCode, blk.code);
END NewBlock;

PROCEDURE OpenBlock(oldBlkCond: INTEGER);
	VAR newBlk: Block;
BEGIN NewBlock(newBlk); newBlk.link := NIL; newBlk.jDst := NIL;
	curBlk.jc := oldBlkCond; curBlk.next := newBlk; curBlk := newBlk
END OpenBlock;

PROCEDURE Put1(n: INTEGER);
BEGIN Sys.WriteMemFile(blk.rCode, n)
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
END CodeLen;

PROCEDURE MergeNextBlock(blk: Block);
	VAR next: Block;
BEGIN ASSERT(blk.jDst = NIL);
	next := blk; Sys.MergeMemFile(blk.code, next.code); blk.next := next.next;
	IF blk.next = NIL THEN curBlk := blk END
END MergeNextBlock;

PROCEDURE MergeFrom(blk: Block);
BEGIN WHILE blk # curBlk DO MergeNextBlock(blk) END
END MergeFrom;
	
(* -------------------------------------------------------------------------- *)
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
	ELSIF x.mode = mBX THEN mem.rm := reg_BX; mem.disp := x.a;
		IF x.a = 0 THEN mem.mod := 0 
		ELSIF (x.a >= -128) & (x.a <= 127) THEN mem.mod := 1
		ELSE mem.mod := 2
		END
	ELSIF x.mode = mRegI THEN SetRmOperand_regI(x.r, x.a)
	ELSIF x.mode = mReg THEN SetRmOperand_reg(x.r)
	ELSIF x.mode = mProc THEN mem.rm := reg_BX; mem.disp := x.a;
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
	SetRm_reg (rm); EmitREX(0, 4); Put1(50H + rm MOD 8); stack := stack + 8
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN
	SetRm_reg (rm); EmitREX(0, 4); Put1(58H + rm MOD 8); stack := stack - 8
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
(* Pass 1 *)

PROCEDURE Pass1(obj: B.Object);
	VAR node: B.Node;
BEGIN
	obj.regUsed := {}; obj.xRegUsed := {};
	IF obj IS B.Node THEN node := obj(B.Node);
		IF (node.op = S.div) OR (node.op = S.mod) THEN
			node.regUsed := node.regUsed + {reg_A, reg_D}
		ELSIF (node.op = S.sproc) & (node.left(B.SProc).id IN B.sfShifts)
			& ~(node.right IS B.Const) THEN
			INCL(node.regUsed, reg_C)
		END;
		IF node.left # NIL THEN Pass1(node.left);
			node.regUsed := node.regUsed + node.left.regUsed
		END;
		IF node.right # NIL THEN Pass1(node.right);
			node.regUsed := node.regUsed + node.right.regUsed
		END;
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

PROCEDURE FixLinkWith(L, dst: Block);
	VAR L1: INTEGER;
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
		REPEAT L2 := L3; L3 := L3.link UNTIL L3 = 0;
		L2.link := L1; L1 := L0
	END;
    RETURN L1
END merged;

PROCEDURE SetCond(VAR x: Item; c: INTEGER);
BEGIN x.mode := mCond; x.aLink := NIL; x.bLink := NIL; x.c := c
END SetCond;

(* -------------------------------------------------------------------------- *)

PROCEDURE ResetMkItmStat;
BEGIN MkItmStat.avoid := {}; MkItmStat.alloc := {}; MkItmStat.bestReg := -1;
	MkItmStat.xAvoid := {}; MkItmStat.xAlloc := {}; MkItmStat.bestXReg := -1
END ResetMkItmStat;

PROCEDURE AllocReg(): INTEGER;
	VAR reg: INTEGER; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.avoid + MkItmStat.alloc + {reg_SP, reg_BP, reg_BX};
	IF (MkItmStat.bestReg = -1) OR (MkItmState.bestReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END
	ELSE reg := MkItmStat.bestReg
	END;
	INCL(MkItmStat.alloc, reg); MkItmStat.avoid := {}
	RETURN reg
END AllocReg;

PROCEDURE AllocReg2(): INTEGER;
	VAR reg: INTEGER; cantAlloc: SET;
BEGIN cantAlloc := MkItmStat.alloc + {reg_SP, reg_BP, reg_BX};
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	RETURN reg
END AllocReg2;

PROCEDURE AllocXReg(): INTEGER;
	VAR reg: INTEGER; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.xAvoid + MkItmStat.xAlloc;
	IF (MkItmStat.bestXReg = -1) OR (MkItmState.bestXReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END
	ELSE reg := MkItmStat.bestXReg
	END;
	INCL(MkItmStat.xAlloc, reg); MkItmStat.xAvoid := {}
	RETURN reg
END AllocXReg;

PROCEDURE AllocXReg2(): INTEGER;
	VAR reg: INTEGER; cantAlloc: SET;
BEGIN cantAlloc := MkItmStat.xAlloc;
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	RETURN reg
END AllocXReg;

PROCEDURE FreeReg(reg: INTEGER);
BEGIN EXCL(MkItmStat.alloc, reg)
END FreeReg;

PROCEDURE FreeXReg(reg: INTEGER);
BEGIN EXCL(MkItmStat.xAlloc, reg)
END FreeXReg;

(* -------------------------------------------------------------------------- *)

PROCEDURE RefToRegI(VAR x: Item);
	VAR reg: INTEGER;
BEGIN
	IF x.ref & (x.mode # mProc) THEN
		ASSERT(x.mode IN {mReg, mRegI, mSP, mIP, mBP});
		IF x.mode IN {mReg, mRegI} THEN reg := x.r ELSE reg := AllocReg() END;
		SetRmOperand(x); EmitRegRm(MOVd, reg, 8);
		x.mode := mRegI; x.a := x.c; x.ref := FALSE
	END
END RefToRegI;

PROCEDURE Load(VAR x: Item);
	VAR r, r2, size: INTEGER; oldType: B.Type;
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

PROCEDURE RelocReg(VAR reg: INTEGER; newReg: INTEGER);
BEGIN EmitRR(MOVd, newReg, 8, reg); FreeReg(reg); reg := newReg
END RelocReg;

PROCEDURE RelocXReg(VAR reg: INTEGER; newReg: INTEGER);
BEGIN SetRm_reg(reg); EmitXmmRm(MOVSD, newReg, 8); FreeXReg(reg); reg := newReg
END RelocReg;

PROCEDURE LoadLeft(VAR x: Item; node: B.Node);
	VAR r: INTEGER; regUsed, xRegUsed: SET;
BEGIN
	IF node.right # NIL THEN
		regUsed := node.right.regUsed; xRegUsed := node.right.xRegUsed;
		MkItmStat.avoid := regUsed; MkItmStat.xAvoid := xRegUsed
	ELSE regUsed := {}; xRegUsed := {}
	END;
	MakeItem0(x, node.left); Load(x);
	IF (x.mode = mReg) & (x.r IN regUsed) THEN
		MkItmStat.avoid := regUsed; RelocReg(x.r, AllocReg())
	ELSIF x.mode = mXReg THEN
		MkItmStat.xAvoid := xRegUsed; RelocReg(x.r, AllocXReg())
	ELSE ASSERT(FALSE)
	END
END LoadLeft;

PROCEDURE LoadRight(VAR y: Item; node: B.Node);
BEGIN MakeItem0(y, node.right); Load(y)
END LoadRight;

PROCEDURE Add(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeft(x, node); LoadRight(y, node);
	IF node.type.form = B.tInt THEN EmitRR(ADDd, x.r, 8, y.r)
	ELSIF node.type.form = B.tSet THEN EmitRR(ORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(ADDSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
END Add;

PROCEDURE Subtract(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeft(x, node);
	IF node.right # NIL THEN LoadRight(y, node);
		IF node.type.form = B.tInt THEN EmitRR(SUBd, x.r, 8, y.r)
		ELSIF node.type.form = B.tSet THEN
			EmitR(NOT, y.r, 8); EmitRR(ANDd, x.r, 8, y.r)
		ELSIF node.type = B.realType THEN
			SetRm_reg(y.r); EmitXmmRm(SUBSD, x.r, 4)
		ELSE ASSERT(FALSE)
		END;
		IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
	ELSIF node.type.form = B.tInt THEN EmitR(NEG, x.r, 8)
	ELSIF node.type.form = B.tSet THEN EmitR(NOT, x.r, 8)
	ELSIF node.type = B.realType THEN
		y.r := AllocXReg2(); SetRm_reg(y.r); EmitXmmRm(XORPD, y.r, 4);
		SetRm_reg(x.r); EmitXmmRm(SUBSD, y.r, 4);
		SetRm_reg(y.r); EmitXmmRm(MOVSD, x.r, 4)
	END
END Subtract;

PROCEDURE Multiply(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeft(x, node); LoadRight(y, node);
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
BEGIN MkItmStat.bestReg := reg_A; LoadLeft(x, node); LoadRight(y, node);
	IF y.r IN divReg THEN
		MkItmStat.avoid := divReg; RelocReg(y.r, AllocReg())
	END;
	IF x.r # reg_A THEN RelocReg(x.r, reg_A); INCL(MkItmStat.alloc, reg_A) END;
	EmitBare(CQO); EmitR(IDIVa, y.r, 8); EmitRR(TEST, reg_D, 8, reg_D);
	blk1 := curBlk; OpenBlock(ccL);
	blk2 := curBlk; OpenBlock(ccAlways); blk1.jDst := curBlk;
	IF node.op = S.div THEN EmitRI(SUBi, reg_A, 8, 1)
	ELSE EmitRR(ADDd, reg_D, 8, y.r)
	END;
	OpenBlock(0); blk2.jDst := curBlk;
	FJump0(blk2); FJump0(blk1); MergeFrom(blk1); FreeReg(y.r);
	IF node.op = S.mod THEN FreeReg(reg_A); x.r := reg_D END
END IntDiv;

PROCEDURE Divide(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeft(x, node); LoadRight(y, node);
	IF node.type.form = B.tSet THEN EmitRR(XORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(DIVSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	IF y.type.form # B.tReal THEN FreeReg(y.r) ELSE FreeXReg(y.r) END
END Divide;

PROCEDURE LoadCond(x: Item; obj: B.Object);
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
END And;

PROCEDURE MakeItem(VAR x: Item; obj: B.Object);
	VAR objv: B.Var; node: B.Node;
BEGIN x.type := obj.type; x.ref := FALSE; x.a := 0; x.b := 0; x.c := 0;
	IF obj IS B.Const THEN x.mode := mImm; x.a := obj(B.Const).val
	ELSIF obj IS B.Var THEN objv := obj(B.Var); x.a := objv.adr;
		IF objv.lev <= 0 THEN x.mode := mIP ELSE x.mode := mSP END;
		IF objv.lev < -1 THEN x.ref := TRUE END;
		IF objv IS B.Str THEN x.strlen := objv(B.Str).len
		ELSIF objv IS B.Par THEN x.par := TRUE;
			IF objv(B.Par).varpar OR objv.ronly THEN ref := TRUE END
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
		staticSize := staticSize + 24 + 8 * (B.MaxExt + q.type.nptr);
		q.type.adr := -staticSize; q := q.next
	END
END AllocStaticData;

PROCEDURE FixGlobalVarAdr;
	VAR p: B.Ident; amount: INTEGER;
BEGIN p := B.universe; amount := staticSize + (-staticSize) MOD 4096;
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
