MODULE Generator1;

IMPORT
	SYSTEM, Sys := BaseSys,
	S := Scanner1, B := Base1;

CONST
	MaxInt = 9223372036854775807;
	MinInt = -MaxInt-1;
	
	MaxSize = 80000000H; (* 2 GB limit *)
	MaxLocBlkSize = 100000H; (* 1 MB limit *)
	
	tempOutputName = 'output.temp_';
	
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
	CMPSB = 0A6H; CMPSW = 0A766H; CMPSD = 0A7H; CMPSQ = 0A748H;
	LODSB = 0ACH; LODSW = 0AD66H; LODSD = 0ADH; LODSQ = 0AD48H;
	STOSB = 0AAH; STOSW = 0AB66H; STOSD = 0ABH; STOSQ = 0AB48H;
	
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
	MOVAPS = 280F00H; MOVAPSd = 290F00H; COMISS = 2F0F00H; COMISD = 2F0F66H;
	CVTSS2SI = 2D0FF3H; CVTSI2SS = 2A0FF3H;
	CVTSD2SI = 2D0FF2H; CVTSI2SD = 2A0FF2H;
	
	(* Item mode *)
	mReg = 0; mXReg = 1; mImm = 2; mRegI = 3; mIP = 4; mSP = 5; mBP = 6;
	mCond = 7; mProc = 8; mType = 9; mBX = 10; mNothing = 11;
	
	(* Trap code *)
	divideTrap = 0;
	arrayTrap = 1;
	typeTrap = 2;
	assertTrap = 3;
	nilTrap = 4;
	modkeyTrap = 5;
	overflowTrap = 6;
	stringTrap = 7;
	arrayLenTrap = 8;
	
TYPE
	Proc = POINTER TO ProcDesc;
	Block = POINTER TO BlockDesc;

	BlockDesc = RECORD
		code: Sys.MemFile; rCode: Sys.MemFileRider;
		jc: BYTE; call, load, finished: BOOLEAN; proc: Proc;
		no, jmpOff: INTEGER; next, link, jDst: Block
	END;
	
	ProcDesc = RECORD
		export: BOOLEAN;
		usedReg, usedXReg: SET; stack, pStk, homeSpace: INTEGER;
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

	curBlk: Block; pc, sPos: INTEGER;
	procList, curProc: Proc; modid: B.IdStr;

	mem: RECORD
		mod, rm, bas, idx, scl, disp: INTEGER
	END;
	allocReg, allocXReg: SET;
	MkItmStat: MakeItemState; (* State for MakeItem procedures in Pass 2 *)
	
	varSize, staticSize: INTEGER;
	
	(* Linker state *)
	Linker: RECORD
		imagebase, entry : INTEGER;
		code_rawsize, data_rawsize, edata_rawsize: INTEGER;
		code_size, data_size, edata_size: INTEGER;
		code_rva, data_rva, idata_rva, reloc_rva, edata_rva: INTEGER;
		code_fadr, data_fadr, idata_fadr, reloc_fadr, edata_fadr: INTEGER;
		bss_size, bss_rva, startTime, endTime: INTEGER;
		Kernel32Table: ARRAY 7 OF INTEGER
	END;
	out: Sys.File;
		
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

PROCEDURE NaturalAlign(VAR size: INTEGER);
BEGIN
	IF size <= 2 THEN (* nothing *)
	ELSIF size <= 4 THEN size := 4
	ELSIF size <= 8 THEN size := 8
	ELSE size := (size + 15) DIV 16 * 16
	END
END NaturalAlign;

PROCEDURE SmallConst(n: INTEGER): BOOLEAN;
	RETURN (n >= -80000000H) & (n < 80000000H)
END SmallConst;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewBlock(VAR blk: Block);
BEGIN NEW(blk); blk.no := 0; blk.jc := 255; blk.jmpOff := 0;
	blk.call := FALSE; blk.load := FALSE; blk.finished := TRUE;
	Sys.NewMemFile(blk.code); Sys.SetMemFile(blk.rCode, blk.code, 0);
END NewBlock;

PROCEDURE OpenBlock(oldBlkCond: INTEGER);
	VAR newBlk: Block;
BEGIN NewBlock(newBlk); newBlk.no := curBlk.no + 1;
	newBlk.link := NIL; newBlk.jDst := NIL; curBlk.jc := oldBlkCond;
	IF oldBlkCond # 255 THEN curBlk.finished := FALSE END;
	curBlk.next := newBlk; curBlk := newBlk
END OpenBlock;

PROCEDURE Put1(n: INTEGER);
BEGIN
	Sys.WriteMemFile(curBlk.rCode, n)
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

PROCEDURE CodeLen1(blk: Block): INTEGER;
	VAR res: INTEGER;
BEGIN res := Sys.MemFileLength(blk.code);
	IF ~blk.finished THEN
		IF blk.load THEN INC(res, 7)
		ELSIF blk.jc = ccAlways THEN INC(res, 5)
		ELSIF blk.jc # ccNever THEN INC(res, 6)
		END
	END;
	RETURN res
END CodeLen1;

PROCEDURE SetCodePos(pos: INTEGER);
BEGIN Sys.SetMemFile(curBlk.rCode, curBlk.code, pos)
END SetCodePos;

PROCEDURE MergeNextBlock(blk: Block);
	VAR next: Block;
BEGIN ASSERT(blk.finished); ASSERT(blk.next.finished);
	next := blk.next; blk.next := next.next;
	Sys.MergeMemFile(blk.code, next.code);
	Sys.SetMemFile(blk.rCode, blk.code, CodeLen0(blk));
	IF blk.next = NIL THEN curBlk := blk END
END MergeNextBlock;

PROCEDURE MergeFrom(blk: Block);
BEGIN
	WHILE blk # curBlk DO MergeNextBlock(blk) END;
	curBlk.jDst := NIL; curBlk.link := NIL; curBlk.jmpOff := 0;
	curBlk.jc := 255; curBlk.call := FALSE; curBlk.load := FALSE
END MergeFrom;

PROCEDURE FindProcBlk(obj: B.Object): Block;
	VAR proc: Proc;
BEGIN proc := procList;
	WHILE proc.obj # obj DO proc := proc.next END;
	RETURN proc.blk
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

PROCEDURE EmitRmImm(op, rsize, imm: INTEGER);
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

PROCEDURE SetRm_RIP(disp: INTEGER);
BEGIN mem.rm := reg_BP; mem.disp := disp; mem.mod := 0
END SetRm_RIP;

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
BEGIN SetRm_reg(rm); EmitREX(0, 4); Put1(50H + rm MOD 8)
END PushR;

PROCEDURE PopR(rm: INTEGER);
BEGIN SetRm_reg(rm); EmitREX(0, 4); Put1(58H + rm MOD 8)
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

PROCEDURE MarkSizeError;
BEGIN S.Mark('Data size limit reached')
END MarkSizeError;

PROCEDURE SetTypeSize*(tp: B.Type);
	VAR size, align, fAlign: INTEGER;
		ident: B.Ident; ftype: B.Type;
BEGIN
	IF tp.size = 0 THEN
		IF tp.form = B.tPtr THEN tp.size := 8; tp.align := 8
		ELSIF tp.form = B.tProc THEN tp.size := 8; tp.align := 8;
			ident := tp.fields; size := 0;
			WHILE ident # NIL DO
				ftype := ident.obj.type; ident.obj(B.Var).adr := size + 16;
				IF B.IsOpenArray(ftype)
				OR (ftype.form = B.tRec) & (ident.obj(B.Par).varpar)
				THEN size := size + 16 ELSE size := size + 8
				END;
				ident := ident.next
			END;
			tp.parblksize := size;
		ELSIF (tp.form = B.tArray) & (tp.len >= 0) THEN
			SetTypeSize(tp.base); tp.align := tp.base.align;
			IF (tp.len # 0) & (MaxSize DIV tp.len < tp.base.size) THEN
				S.Mark('Size of type is too large'); tp.size := tp.align
			ELSE tp.size := tp.base.size * tp.len
			END
		ELSIF tp.form = B.tRec THEN
			IF tp.base # NIL THEN SetTypeSize(tp.base);
				size := tp.base.size; align := tp.base.align
			ELSE size := 0; align := 0
			END;
			ident := tp.fields;
			WHILE ident # NIL DO ftype := ident.obj.type;
				SetTypeSize(ftype); fAlign := ftype.align;
				IF fAlign > align THEN align := fAlign END;
				size := (size + fAlign - 1) DIV fAlign * fAlign;
				ident.obj(B.Field).off := size; INC(size, ftype.size);
				IF size > MaxSize THEN
					S.Mark('Size of type is too large'); size := align
				END;
				ident := ident.next
			END;
			tp.size := size; tp.align := align
		ELSE ASSERT(FALSE)
		END
	END
END SetTypeSize;

PROCEDURE SetGlobalVarSize*(x: B.Object);
	VAR align: INTEGER;
BEGIN align := x.type.align;
	varSize := (varSize + align - 1) DIV align * align;
	varSize := varSize + x.type.size; x(B.Var).adr := -varSize;
	IF varSize > MaxSize THEN varSize := 0; MarkSizeError END
END SetGlobalVarSize;

PROCEDURE SetProcVarSize*(proc: B.Proc; x: B.Object);
	VAR size, align: INTEGER;
BEGIN size := proc.locblksize; align := x.type.align;
	size := (size + align - 1) DIV align * align + x.type.size;
	x(B.Var).adr := -size; proc.locblksize := size;
	IF size > MaxLocBlkSize THEN proc.locblksize := 0; MarkSizeError END
END SetProcVarSize;

PROCEDURE AllocImportModules*;
	VAR i, j, size: INTEGER; imod: B.Module;
BEGIN i := 0;
	WHILE i < B.modno DO
		imod := B.modList[i]; j := 0; WHILE imod.name[j] # 0X DO INC(j) END;
		size := ((j+5)*2 + 15) DIV 16 * 16; INC(staticSize, size);
		imod.adr := -staticSize; INC(i)
	END
END AllocImportModules;

PROCEDURE AllocImport*(x: B.Object; module: B.Module);
	VAR p: B.Ident;
BEGIN
	NEW(p); p.obj := x; p.next := module.impList;
	module.impList := p; INC(staticSize, 8);
	IF x IS B.Var THEN x(B.Var).adr := -staticSize
	ELSIF x IS B.Proc THEN x(B.Proc).adr := -staticSize
	ELSIF x.class = B.cType THEN
		IF x.type.form = B.tRec THEN x.type.adr := -staticSize
		ELSIF x.type.form = B.tPtr THEN x.type.base.adr := -staticSize
		END
	END
END AllocImport;

PROCEDURE AllocStaticData;
	VAR p: B.Ident; q: B.TypeList; x: B.Object;
		strSize, tdSize, align: INTEGER;
BEGIN p := B.strList;
	WHILE p # NIL DO
		x := p.obj; strSize := 2*x(B.Str).len;
		NaturalAlign(strSize); INC(staticSize, strSize);
		IF strSize <= 8 THEN align := strSize ELSE align := 16 END;
		staticSize := (staticSize + align - 1) DIV align * align;
		x(B.Str).adr := -staticSize; p := p.next
	END;
	staticSize := (staticSize + 15) DIV 16 * 16; q := B.recList;
	WHILE q # NIL DO
		tdSize := (24 + 8*(B.MaxExt + q.type.nptr)) DIV 16 * 16;
		q.a := tdSize; INC(staticSize, tdSize);
		q.type.adr := -staticSize; q := q.next
	END
END AllocStaticData;

PROCEDURE Pass1(obj: B.Object);
	VAR node, par1, par2: B.Node; fpar: B.Ident; e: INTEGER;
	
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
		IF par.right # NIL THEN
			ParNode(par.right(B.Node), fpar.next, n);
			par.regUsed := par.regUsed + par.right.regUsed;
			par.xRegUsed := par.xRegUsed + par.right.xRegUsed
		END
	END ParNode;
	
BEGIN (* Pass1 *)
	obj.regUsed := {}; obj.xRegUsed := {};
	IF obj IS B.Node THEN node := obj(B.Node);
		IF node.op # S.call THEN
			IF node.left # NIL THEN Pass1(node.left);
				node.regUsed := node.left.regUsed;
				node.xRegUsed := node.left.xRegUsed
			END;
			IF node.right # NIL THEN Pass1(node.right);
				node.regUsed := node.regUsed + node.right.regUsed;
				node.xRegUsed := node.xRegUsed + node.right.xRegUsed
			END;
			IF node.op = S.times THEN
				IF (node.type = B.intType) & (node.right IS B.Const) THEN
					e := log2(node.right(B.Const).val);
					IF e >= 0 THEN
						node.op := S.sfLSL;
						node.right := B.NewConst(B.intType, e)
					END
				END
			ELSIF (node.op = S.div) OR (node.op = S.mod) THEN
				IF node.right IS B.Const THEN
					e := log2(node.right(B.Const).val);
					IF e >= 0 THEN
						IF node.op = S.div THEN
							node.op := S.sfASR;
							node.right := B.NewConst(B.intType, e)		
						END
					ELSE node.regUsed := node.regUsed + {reg_A, reg_D}
					END
				ELSE node.regUsed := node.regUsed + {reg_A, reg_D}
				END
			ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
				IF ~(node.right IS B.Const) THEN INCL(node.regUsed, reg_C) END
			ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
				IF B.IsStr(node.left.type) THEN
					node.regUsed := node.regUsed + {reg_SI, reg_DI}
				END
			ELSIF node.op = S.upto THEN INCL(node.regUsed, reg_C)
			ELSIF node.op = S.becomes THEN
				IF node.left.type.form IN {B.tArray, B.tRec} THEN
					node.regUsed := node.regUsed + {reg_SI, reg_DI}
				END
			ELSIF node.op = S.semicolon THEN
				node.regUsed := {}; node.xRegUsed := {}
			END
		ELSE Pass1(node.left);
			node.regUsed :=
				{reg_A, reg_C, reg_D, reg_R8, reg_R9, reg_R10, reg_R11};
			node.xRegUsed := {0..5};
			node.regUsed := node.regUsed + node.left.regUsed;
			node.xRegUsed := node.xRegUsed + node.left.xRegUsed;
			fpar := node.left.type.fields;
			IF fpar # NIL THEN
				ParNode(node.right(B.Node), fpar, 0);
				node.regUsed := node.regUsed + node.right.regUsed;
				node.xRegUsed := node.xRegUsed + node.right.xRegUsed
			END
		END
	END
END Pass1;

PROCEDURE ScanDeclaration(decl: B.Ident; lev: INTEGER);
	VAR obj: B.Object; procObj: B.Proc; proc: Proc;
		fixAmount: INTEGER;
BEGIN
	WHILE decl # NIL DO obj := decl.obj;
		IF obj IS B.Proc THEN
			procObj := obj(B.Proc); ScanDeclaration(procObj.decl, lev+1);
			NEW(proc); proc.export := decl.export;
			proc.obj := procObj; NewBlock(proc.blk);
			IF curProc = NIL THEN procList := proc; curProc := proc
			ELSE curProc.next := proc; curProc := proc
			END;
			IF procObj.statseq # NIL THEN Pass1(procObj.statseq) END;
			IF procObj.return # NIL THEN Pass1(procObj.return) END
		ELSIF (lev = 0) & (obj IS B.Var) & ~(obj IS B.Str) THEN
			(* fix global var address *)
			fixAmount := (staticSize + 4095) DIV 4096 * 4096;
			DEC(obj(B.Var).adr, fixAmount)
		END;
		decl := decl.next
	END
END ScanDeclaration;

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
		ASSERT(blk.finished); INC(off, CodeLen0(blk)); blk := blk.next
	END;
	IF (off > 0) & (cc # ccNever) THEN
		src.jmpOff := off; blk := curBlk; curBlk := src; 
		IF cc = ccAlways THEN
			IF off <= 127 THEN Branch1(off) ELSE Branch(off) END
		ELSIF off <= 127 THEN CondBranch1(cc, off) ELSE CondBranch(cc, off)
		END;
		curBlk := blk
	END;
	src.finished := TRUE
END FJump0;

PROCEDURE FJump(src: Block);
	VAR off, cc: INTEGER; blk: Block;
BEGIN off := 0; blk := src.next; cc := src.jc;
	WHILE blk # src.jDst DO INC(off, CodeLen1(blk)); blk := blk.next END;
	IF (off > 0) & (cc # ccNever) THEN
		src.jmpOff := off; blk := curBlk; curBlk := src; 
		IF cc = ccAlways THEN
			IF off <= 127 THEN Branch1(off) ELSE Branch(off) END
		ELSIF off <= 127 THEN CondBranch1(cc, off) ELSE CondBranch(cc, off)
		END;
		curBlk := blk
	END;
	src.finished := TRUE
END FJump;

PROCEDURE BJump(src: Block);
	VAR off, cc: INTEGER; blk: Block;
BEGIN off := -CodeLen0(src); blk := src.jDst; cc := src.jc;
	WHILE blk # src DO DEC(off, CodeLen1(blk)); blk := blk.next END;
	IF off-2 >= -128 THEN DEC(off, 2)
	ELSIF cc = ccAlways THEN DEC(off, 5) ELSE DEC(off, 6)
	END;
	IF cc # ccNever THEN
		src.jmpOff := off; blk := curBlk; curBlk := src;
		IF cc = ccAlways THEN
			IF off >= -128 THEN Branch1(off) ELSE Branch(off) END
		ELSIF off >= -128 THEN CondBranch1(cc, off) ELSE CondBranch(cc, off)
		END;
		curBlk := blk
	END;
	src.finished := TRUE
END BJump;

PROCEDURE FixLinkWith(L, dst: Block);
	VAR L1: Block;
BEGIN
	WHILE L # NIL DO
		L.jDst := dst; IF dst.no <= L.no THEN BJump(L) ELSE FJump(L) END;
		L1 := L.link; L.link := NIL; L := L1
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
	IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccB ELSIF op = S.gtr THEN op := ccA
	ELSIF op = S.leq THEN op := ccBE ELSE op := ccAE
	END;
	RETURN op
END OpToCc;

PROCEDURE IntOpToCc(op: INTEGER): INTEGER;
BEGIN
	IF op = S.eql THEN op := ccZ ELSIF op = S.neq THEN op := ccNZ
	ELSIF op = S.lss THEN op := ccL ELSIF op = S.gtr THEN op := ccG
	ELSIF op = S.leq THEN op := ccLE ELSE op := ccGE
	END;
	RETURN op
END IntOpToCc;

PROCEDURE Trap(cond, trapno: INTEGER);
	VAR blk1, blk2: Block;
BEGIN
	IF ~(cond IN {ccAlways, ccNever}) THEN
		blk1 := curBlk; OpenBlock(cond);
		blk2 := curBlk; OpenBlock(ccAlways); blk1.jDst := curBlk;
		MoveRI(reg_A, 1, trapno); MoveRI(reg_C, 4, sPos); EmitBare(INT3);
		OpenBlock(255); blk2.jDst := curBlk; FJump0(blk2); FJump0(blk1);
		MergeFrom(blk1)
	ELSIF cond = ccAlways THEN
		MoveRI(reg_A, 1, trapno); MoveRI(reg_C, 4, sPos); EmitBare(INT3)
	END
END Trap;

(* -------------------------------------------------------------------------- *)

PROCEDURE ResetMkItmStat;
BEGIN
	MkItmStat.avoid := {}; MkItmStat.bestReg := 255;
	MkItmStat.xAvoid := {}; MkItmStat.bestXReg := 255
END ResetMkItmStat;

PROCEDURE ResetMkItmStat2(VAR oldStat: MakeItemState);
BEGIN oldStat := MkItmStat;
	MkItmStat.avoid := {}; MkItmStat.bestReg := 255;
	MkItmStat.xAvoid := {}; MkItmStat.bestXReg := 255
END ResetMkItmStat2;

PROCEDURE SetAlloc(reg: BYTE);
BEGIN INCL(allocReg, reg); INCL(curProc.usedReg, reg)
END SetAlloc;

PROCEDURE SetAllocX(reg: BYTE);
BEGIN INCL(allocXReg, reg); INCL(curProc.usedXReg, reg)
END SetAllocX;

PROCEDURE AllocReg(): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN
	cantAlloc := MkItmStat.avoid + allocReg + {reg_SP, reg_BP, reg_B};
	IF (MkItmStat.bestReg = 255) OR (MkItmStat.bestReg IN cantAlloc) THEN
		reg := 0; WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 3 THEN reg := 8;
			WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
			IF reg >= 12 THEN reg := 6;
				WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
				IF reg >= 16 THEN S.Mark('Reg stack overflow') END
			END
		END
	ELSE reg := MkItmStat.bestReg
	END;
	ASSERT(reg < 16); SetAlloc(reg);
	RETURN reg
END AllocReg;

PROCEDURE AllocReg2(avoid: SET): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := avoid + allocReg + {reg_SP, reg_BP, reg_B};
	reg := 0; WHILE (reg < 3) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 3 THEN reg := 8;
		WHILE (reg < 12) & (reg IN cantAlloc) DO INC(reg) END;
		IF reg >= 12 THEN reg := 6;
			WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
			IF reg >= 16 THEN S.Mark('Reg stack overflow') END
		END
	END;
	ASSERT(reg < 16); SetAlloc(reg);
	RETURN reg
END AllocReg2;

PROCEDURE SetAvoid(reg: BYTE);
BEGIN INCL(MkItmStat.avoid, reg)
END SetAvoid;

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
	SetAllocX(reg);
	RETURN reg
END AllocXReg;

PROCEDURE AllocXReg2(avoid: SET): BYTE;
	VAR reg: BYTE; cantAlloc: SET;
BEGIN cantAlloc := avoid + allocXReg;
	reg := 0; WHILE (reg < 16) & (reg IN cantAlloc) DO INC(reg) END;
	IF reg >= 16 THEN S.Mark('Reg stack overflow'); ASSERT(FALSE) END;
	SetAllocX(reg);
	RETURN reg
END AllocXReg2;

PROCEDURE FreeReg(reg: BYTE);
BEGIN EXCL(allocReg, reg)
END FreeReg;

PROCEDURE FreeXReg(reg: BYTE);
BEGIN EXCL(allocXReg, reg)
END FreeXReg;

PROCEDURE FreeReg2(x: Item);
BEGIN
	IF x.mode IN {mReg, mRegI} THEN FreeReg(x.r)
	ELSIF x.mode = mXReg THEN FreeXReg(x.r)
	END
END FreeReg2;

(* -------------------------------------------------------------------------- *)

PROCEDURE RelocReg(VAR reg: BYTE; newReg: BYTE);
BEGIN
	EmitRR(MOVd, newReg, 8, reg);
	FreeReg(reg); reg := newReg; SetAlloc(reg)
END RelocReg;

PROCEDURE RelocXReg(VAR reg: BYTE; newReg: BYTE);
BEGIN
	SetRm_reg(reg); EmitXmmRm(MOVSD, newReg, 8);
	FreeXReg(reg); reg := newReg; SetAllocX(reg)
END RelocXReg;

PROCEDURE RefToRegI(VAR x: Item);
	VAR reg: BYTE;
BEGIN
	IF x.ref & (x.mode # mProc) THEN
		ASSERT(x.mode IN {mSP, mIP, mBP, mBX});
		reg := AllocReg(); SetRmOperand(x); EmitRegRm(MOVd, reg, 8);
		x.mode := mRegI; x.r := reg; x.a := x.b; x.ref := FALSE
	END
END RefToRegI;

PROCEDURE LoadImm(r: BYTE; size, imm: INTEGER);
BEGIN
	IF imm = 0 THEN EmitRR(XOR, r, 4, r)
	ELSIF size <= 4 THEN MoveRI(r, size, imm)
	ELSIF (imm > 0) & (imm < 100000000H) THEN MoveRI(r, 4, imm)
	ELSIF SmallConst(imm) THEN EmitRR(XOR, r, 4, r); EmitRI(ADDi, r, 8, imm)
	ELSE MoveRI(r, 8, imm)
	END
END LoadImm;

PROCEDURE Load(VAR x: Item);
	VAR r, r2: BYTE; size: INTEGER; oldType: B.Type; blk: Block;
BEGIN RefToRegI(x);
	IF x.type.form # B.tReal THEN
		IF x.mode # mReg THEN size := x.type.size;
			IF x.mode # mRegI THEN r := AllocReg() ELSE r := x.r END;
			IF x.mode = mImm THEN LoadImm(r, size, x.a)
			ELSIF x.mode IN {mRegI, mSP, mIP, mBP, mBX} THEN
				SetRmOperand(x);
				IF size >= 4 THEN EmitRegRm(MOVd, r, size)
				ELSIF x.type = B.strType THEN
					ASSERT(x.strlen <= 2); EmitMOVZX(r, 2)
				ELSE EmitMOVZX(r, size)
				END
			ELSIF x.mode = mProc THEN
				IF ~x.ref THEN blk := curBlk; OpenBlock(r);
					blk.jDst := x.aLink; blk.load := TRUE
				ELSE SetRmOperand(x); EmitRegRm(MOVd, r, 8); x.ref := FALSE
				END
			ELSIF x.mode = mCond THEN
				curBlk.link := x.aLink; x.aLink := curBlk;
				OpenBlock(negated(x.c)); FixLink(x.bLink);
				MoveRI(r, 4, 1); blk := curBlk; OpenBlock(ccAlways);
				FixLink(x.aLink); EmitRR(XOR, r, 4, r); OpenBlock(255);
				blk.jDst := curBlk; FJump(blk)
			ELSE ASSERT(FALSE)
			END;
			x.mode := mReg; x.r := r
		END
	ELSIF x.mode # mXReg THEN r := AllocXReg();
		IF x.mode = mImm THEN
			IF x.a = 0 THEN SetRm_reg(r); EmitXmmRm(XORPS, r, 4)
			ELSE r2 := AllocReg2({});
				LoadImm(r2, x.type.size, x.a); SetRm_reg(r2);
				EmitXmmRm(SseMOVD, r, x.type.size); FreeReg(r2)
			END
		ELSE SetRmOperand(x); EmitXmmRm(MOVSD, r, 4)
		END;
		FreeReg2(x); x.mode := mXReg; x.r := r
	END
END Load;

PROCEDURE LoadAdr(VAR x: Item);
	VAR r: BYTE;
BEGIN
	RefToRegI(x); SetRmOperand(x);
	IF x.mode = mRegI THEN r := x.r ELSE r := AllocReg() END;
	IF (x.mode # mRegI) OR (x.a # 0) THEN EmitRegRm(LEA, r, 8) END;
	x.r := r; x.mode := mReg
END LoadAdr;

PROCEDURE ArrayLen(VAR x: Item; obj: B.Object);
BEGIN
	IF obj IS B.Str THEN x.mode := mImm; x.a := obj(B.Str).len
	ELSIF B.IsOpenArray(obj.type) THEN MakeItem0(x, obj); INC(x.a, 8)
	ELSIF B.IsNormalArray(obj.type) THEN x.mode := mImm; x.a := obj.type.len
	ELSE ASSERT(FALSE)
	END;
	x.type := B.intType; x.ref := FALSE
END ArrayLen;

PROCEDURE SizeOf(VAR x: Item; obj: B.Object);
	VAR size, e: INTEGER;
BEGIN
	IF obj IS B.Str THEN x.mode := mImm; x.a := obj(B.Str).len*2
	ELSIF B.IsOpenArray(obj.type) THEN size := obj.type.base.size;
		IF size = 0 THEN x.mode := mImm; x.a := 0
		ELSE MakeItem0(x, obj); INC(x.a, 8); Load(x); e := log2(size);
			IF e > 0 THEN EmitRI(SHLi, x.r, 8, e) END
		END
	ELSE x.mode := mImm; x.a := obj.type.size
	END;
	x.type := B.intType; x.ref := FALSE
END SizeOf;

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
		tag.r := AllocReg(); SetRmOperand(x);
		EmitRegRm(MOVd, tag.r, 8); tag.mode := mReg
	END; TypeTag(tag)
END TypeTag2;

PROCEDURE TypeDesc(VAR x: Item; tp: B.Type);
BEGIN
	IF tp.form = B.tRec THEN x.a := tp.adr ELSE ASSERT(FALSE) END;
	x.mode := mBX; x.type := B.intType; x.b := 0; x.ref := tp.lev < 0
END TypeDesc;

PROCEDURE AvoidUsedBy(obj: B.Object);
BEGIN
	IF obj # NIL THEN
		IF (obj.regUsed # {}) OR (obj.xRegUsed # {}) THEN
			MkItmStat.avoid := MkItmStat.avoid + obj.regUsed;
			MkItmStat.xAvoid := MkItmStat.xAvoid + obj.xRegUsed
		END
	END
END AvoidUsedBy;

PROCEDURE LoadLeftRight(VAR x, y: Item; node: B.Node);
	VAR oldStat: MakeItemState;
BEGIN
	oldStat := MkItmStat; AvoidUsedBy(node.right);
	MakeItem0(x, node.left); Load(x);
	ResetMkItmStat; MakeItem0(y, node.right); Load(y); MkItmStat := oldStat
END LoadLeftRight;

PROCEDURE LoadLeftRight2(VAR x, y: Item; node: B.Node);
BEGIN
	AvoidUsedBy(node.right); MakeItem0(x, node.left); Load(x);
	ResetMkItmStat; MakeItem0(y, node.right); Load(y)
END LoadLeftRight2;

PROCEDURE Add(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tInt THEN EmitRR(ADDd, x.r, 8, y.r)
	ELSIF node.type.form = B.tSet THEN EmitRR(ORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(ADDSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	FreeReg2(y)
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
		FreeReg2(y)
	ELSE MakeItem0(x, node.left); Load(x);
		IF node.type.form = B.tInt THEN EmitR(NEG, x.r, 8)
		ELSIF node.type.form = B.tSet THEN EmitR(NOT, x.r, 8)
		ELSIF node.type = B.realType THEN y.r := AllocXReg2({});
			SetRm_reg(y.r); EmitXmmRm(XORPS, y.r, 4);
			SetRm_reg(x.r); EmitXmmRm(SUBSD, y.r, 4);
			SetRm_reg(y.r); EmitXmmRm(MOVSD, x.r, 4); FreeXReg(y.r)
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
	FreeReg2(y)
END Multiply;

PROCEDURE IntDiv(VAR x: Item; node: B.Node);
	VAR y: Item; blk1, blk2: Block;
		oldStat: MakeItemState; n: INTEGER;
BEGIN
	IF (node.right IS B.Const) & (log2(node.right(B.Const).val) >= 0) THEN
		ASSERT(node.op = S.mod);
		MakeItem0(x, node.left); Load(x); n := node.right(B.Const).val - 1;
		IF ~SmallConst(n) THEN
			MakeItem0(y, node.right); DEC(y.a); Load(y);
			EmitRR(ANDd, x.r, 8, y.r); FreeReg(y.r)
		ELSE EmitRI(ANDi, x.r, 8, n)
		END
	ELSE
		ResetMkItmStat2(oldStat); AvoidUsedBy(node.right);
		SetBestReg(reg_A); MakeItem0(x, node.left); Load(x);
		ResetMkItmStat; SetAvoid(reg_A); SetAvoid(reg_D);
		MakeItem0(y, node.right); Load(y);
		IF x.r # reg_A THEN RelocReg(x.r, reg_A) END; SetAlloc(reg_D);
		
		EmitBare(CQO); EmitR(IDIVa, y.r, 8); EmitRR(TEST, reg_D, 8, reg_D);
		blk1 := curBlk; OpenBlock(ccL);
		blk2 := curBlk; OpenBlock(ccAlways); blk1.jDst := curBlk;
		IF node.op = S.div THEN EmitRI(SUBi, reg_A, 8, 1)
		ELSE EmitRR(ADDd, reg_D, 8, y.r)
		END;
		OpenBlock(255); blk2.jDst := curBlk;
		FJump0(blk2); FJump0(blk1); MergeFrom(blk1);
		
		IF node.op = S.div THEN FreeReg(reg_D)
		ELSE FreeReg(reg_A); x.r := reg_D
		END;
		FreeReg(y.r); MkItmStat := oldStat
	END
END IntDiv;

PROCEDURE Divide(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN LoadLeftRight(x, y, node);
	IF node.type.form = B.tSet THEN EmitRR(XORd, x.r, 8, y.r)
	ELSIF node.type = B.realType THEN SetRm_reg(y.r); EmitXmmRm(DIVSD, x.r, 4)
	ELSE ASSERT(FALSE)
	END;
	FreeReg2(y)
END Divide;

PROCEDURE LoadCond(VAR x: Item; obj: B.Object);
	VAR oldStat: MakeItemState;
BEGIN ResetMkItmStat2(oldStat); MakeItem0(x, obj);
	IF x.mode # mCond THEN 
		IF x.mode = mImm THEN SetCond(x, ccNever - x.a)
		ELSE Load(x); EmitRR(TEST, x.r, 4, x.r); FreeReg(x.r); SetCond(x, ccNZ)
		END
	END;
	MkItmStat := oldStat
END LoadCond;

PROCEDURE And(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN
	LoadCond(x, node.left); curBlk.link := x.aLink; x.aLink := curBlk;
	OpenBlock(negated(x.c)); FixLink(x.bLink); LoadCond(y, node.right);
	x.aLink := merged(y.aLink, x.aLink); x.bLink := y.bLink; x.c := y.c
END And;

PROCEDURE Or(VAR x: Item; node: B.Node);
	VAR y: Item;
BEGIN
	LoadCond(x, node.left); curBlk.link := x.bLink; x.bLink := curBlk;
	OpenBlock(x.c); FixLink(x.aLink); LoadCond(y, node.right);
	x.bLink := merged(y.bLink, x.bLink); x.aLink := y.aLink; x.c := y.c
END Or;

PROCEDURE Not(VAR x: Item; node: B.Node);
	VAR t: Block;
BEGIN
	LoadCond(x, node.left); x.c := negated(x.c);
	t := x.aLink; x.aLink := x.bLink; x.bLink := t
END Not;

PROCEDURE Compare(VAR x: Item; node: B.Node);
	VAR cond: INTEGER; cx, r: BYTE; oldStat: MakeItemState;
		y, len: Item; tp: B.Type; blk1, blk2, blk3: Block;		
BEGIN ResetMkItmStat2(oldStat); tp := node.left.type;
	IF tp.form IN B.typScalar - {B.tReal} THEN
		LoadLeftRight2(x, y, node); EmitRR(CMPd, x.r, 8, y.r);
		IF x.type.form = B.tInt THEN cond := IntOpToCc(node.op)
		ELSE cond := OpToCc(node.op)
		END;
		FreeReg(x.r); FreeReg(y.r); SetCond(x, cond)
	ELSIF tp = B.realType THEN
		LoadLeftRight2(x, y, node); SetRm_reg(y.r); EmitXmmRm(COMISD, x.r, 4);
		FreeXReg(x.r); FreeXReg(y.r); SetCond(x, IntOpToCc(node.op))
	ELSIF B.IsStr(tp) THEN
		SetBestReg(reg_SI); AvoidUsedBy(node.right); SetAvoid(reg_DI);
		MakeItem0(x, node.left); LoadAdr(x); ResetMkItmStat;
		SetBestReg(reg_DI); MakeItem0(y, node.right); LoadAdr(y);
		IF y.r # reg_DI THEN RelocReg(y.r, reg_DI) END;
		IF x.r # reg_SI THEN RelocReg(x.r, reg_SI) END;
		
		cx := AllocReg2({}); EmitRR(XOR, cx, 4, cx);
		OpenBlock(255); blk1 := curBlk; EmitRI(ADDi, cx, 8, 1);
		
		ArrayLen(len, node.left);
		IF len.mode = mImm THEN
			IF SmallConst(len.a) THEN EmitRI(CMPi, cx, 8, len.a)
			ELSE Load(len); EmitRR(CMPd, cx, 8, len.r); FreeReg(len.r)
			END
		ELSE SetRmOperand(len); EmitRegRm(CMPd, cx, 8); FreeReg2(len)
		END;
		Trap(ccA, stringTrap);
		
		ArrayLen(len, node.right);
		IF len.mode = mImm THEN
			IF SmallConst(len.a) THEN EmitRI(CMPi, cx, 8, len.a)
			ELSE Load(len); EmitRR(CMPd, cx, 8, len.r); FreeReg(len.r)
			END
		ELSE SetRmOperand(len); EmitRegRm(CMPd, cx, 8); FreeReg2(len)
		END;
		Trap(ccA, stringTrap);
		
		EmitBare(CMPSW); OpenBlock(ccNZ); blk2 := curBlk;
		SetRm_regI(reg_SI, -2); EmitRmImm(CMPi, 2, 0); OpenBlock(ccNZ);
		blk1.jDst := curBlk; blk2.jDst := blk1; FJump(blk1); BJump(blk2);
		FreeReg(reg_DI); FreeReg(reg_SI); SetCond(x, OpToCc(node.op))
	ELSE ASSERT(FALSE)
	END;
	MkItmStat := oldStat
END Compare;

PROCEDURE MemberTest(VAR x: Item; node: B.Node);
	VAR y: Item; oldStat: MakeItemState;
BEGIN ResetMkItmStat2(oldStat);
	IF node.left IS B.Const THEN
		MakeItem0(x, node.left); MakeItem0(y, node.right); RefToRegI(y);
		SetRmOperand(y); EmitRmImm(BTi, 8, x.a MOD 64); FreeReg2(y)
	ELSE
		LoadLeftRight2(x, y, node);
		EmitRR(BT, x.r, 8, y.r); FreeReg(x.r); FreeReg(y.r)
	END;
	SetCond(x, ccC); MkItmStat := oldStat
END MemberTest;

PROCEDURE TypeTest(VAR x: Item; node: B.Node);
	VAR y, tag: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN
	ResetMkItmStat2(oldStat); tp := node.right.type;
	IF tp.form = B.tPtr THEN tp := tp.base END;
	MakeItem0(x, node.left); TypeTag(x); Load(x);
	TypeDesc(y, tp); LoadAdr(y); SetRm_regI(x.r, tp.len * 8);
	EmitRegRm(CMP, y.r, 8); FreeReg(x.r); FreeReg(y.r);
	SetCond(x, ccZ); MkItmStat := oldStat
END TypeTest;

PROCEDURE Deref(VAR x: Item; node: B.Node);
BEGIN
	MakeItem0(x, node.left); Load(x);
	(*EmitRR(TEST, x.r, 8, x.r); Trap(ccZ, nilTrap);*)
	x.mode := mRegI; x.a := 0
END Deref;

PROCEDURE Field(VAR x: Item; node: B.Node);
	VAR off: INTEGER;
BEGIN
	MakeItem0(x, node.left); off := node.right(B.Field).off;
	IF x.ref THEN INC(x.b, off) ELSE INC(x.a, off) END
END Field;

PROCEDURE TypeCheck(VAR x: Item; node: B.Node);
	VAR tag, y: Item; oldStat: MakeItemState; tp: B.Type;
BEGIN
	MakeItem0(x, node.left); ResetMkItmStat2(oldStat);
	tp := node.right.type; IF tp.form = B.tPtr THEN tp := tp.base END;
	TypeDesc(y, tp); LoadAdr(y); TypeTag2(tag, x);
	Load(tag); SetRm_regI(tag.r, tp.len * 8); EmitRegRm(CMP, y.r, 8);
	FreeReg(tag.r); FreeReg(y.r); Trap(ccNZ, typeTrap); MkItmStat := oldStat
END TypeCheck;

PROCEDURE Index(VAR x: Item; node: B.Node);
	VAR idx, size, align, e: INTEGER; len, y: Item; bType: B.Type;
		oldStat: MakeItemState;
BEGIN
	oldStat := MkItmStat; AvoidUsedBy(node.right);
	MakeItem0(x, node.left); bType := x.type.base;
	align := bType.align; size := (bType.size + align - 1) DIV align * align;
	IF node.right IS B.Const THEN idx := node.right(B.Const).val;
		IF B.IsOpenArray(x.type) THEN
			ArrayLen(len, node.left); SetRmOperand(len);
			EmitRmImm(CMPi, 8, idx); Trap(ccBE, arrayTrap)
		END;
		IF x.ref THEN INC(x.b, idx*size) ELSE INC(x.a, idx*size) END
	ELSE RefToRegI(x);
		IF x.mode # mRegI THEN LoadAdr(x); x.mode := mRegI; x.a := 0 END;
		ResetMkItmStat; MakeItem0(y, node.right); Load(y);
		ArrayLen(len, node.left);
		IF len.mode = mImm THEN EmitRI(CMPi, y.r, 8, len.a)
		ELSE SetRmOperand(len); EmitRegRm(CMPd, y.r, 8)
		END;
		Trap(ccAE, arrayTrap);
		IF size > 0 THEN e := log2(size);
			IF e > 0 THEN EmitRI(SHLi, y.r, 8, e)
			ELSIF e < 0 THEN EmitRI(IMULi, y.r, 8, size)
			END
		ELSE EmitRR(XOR, y.r, 4, y.r)
		END;
		EmitRR(ADDd, x.r, 8, y.r); FreeReg(y.r)
	END;
	MkItmStat := oldStat
END Index;

PROCEDURE SingletonSet(VAR x: Item; node: B.Node);
	VAR r: INTEGER; oldStat: MakeItemState;
BEGIN
	r := AllocReg(); ResetMkItmStat2(oldStat); MakeItem0(x, node.left);
	Load(x); EmitRR(XOR, r, 4, r); EmitRR(BTS, x.r, 8, r);
	FreeReg(x.r); x.r := r; MkItmStat := oldStat
END SingletonSet;

PROCEDURE RangeSet(VAR x: Item; node: B.Node);
	VAR r, r2: INTEGER; oldStat: MakeItemState;
BEGIN oldStat := MkItmStat; SetAvoid(reg_C);
	r := AllocReg(); r2 := AllocReg2({reg_C}); ResetMkItmStat;
	SetBestReg(reg_C); MakeItem0(x, node.left); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C) END;
	MoveRI(r, 8, -1); EmitR(SHLcl, r, 8); FreeReg(reg_C);
	SetBestReg(reg_C); MakeItem0(x, node.right); Load(x);
	IF x.r # reg_C THEN RelocReg(x.r, reg_C) END;
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

PROCEDURE LoadParam(VAR x: Item; par: B.Node; n: INTEGER; ref: BOOLEAN);
	VAR obj: B.Object;
BEGIN
	IF n < 4 THEN AvoidUsedBy(par.right) END; obj := par.left;
	IF ~ref THEN
		IF obj.type.form # B.tReal THEN SetBestReg(ParReg(n))
		ELSIF n < 4 THEN MkItmStat.bestXReg := n
		END;
		MakeItem0(x, obj); Load(x)
	ELSE SetBestReg(ParReg(n)); MakeItem0(x, obj); LoadAdr(x)
	END;
	IF n >= 4 THEN SetRm_regI(reg_SP, n*8);
		IF x.mode = mReg THEN EmitRegRm(MOV, x.r, 8); FreeReg(x.r)
		ELSE EmitXmmRm(MOVSDd, x.r, 8); FreeXReg(x.r)
		END
	END
END LoadParam;

PROCEDURE Parameter(par: B.Node; fpar: B.Ident; n: INTEGER);
	VAR varpar: BOOLEAN; ftype: B.Type; x, y: Item; i: INTEGER; r: BYTE;
BEGIN i := 1;
	ResetMkItmStat; ftype := fpar.obj.type; varpar := fpar.obj(B.Par).varpar;
	IF ftype.form = B.tArray THEN LoadParam(x, par, n, TRUE);
		IF B.IsOpenArray(ftype) THEN INC(i) END
	ELSIF ftype = B.strType THEN LoadParam(x, par, n, TRUE)
	ELSIF ftype.form = B.tRec THEN LoadParam(x, par, n, TRUE);
		IF varpar THEN INC(i) END
	ELSE LoadParam(x, par, n, varpar)
	END;
	IF par.right # NIL THEN Parameter(par.right(B.Node), fpar.next, n+i) END;
	IF n < 4 THEN
		IF x.mode = mReg THEN r := ParReg(n);
			IF x.r # r THEN RelocReg(x.r, r) END
		ELSIF x.r # n THEN RelocXReg(x.r, n)
		END
	END;
	IF i = 2 THEN ResetMkItmStat; SetBestReg(ParReg(n+1));
		IF ftype.form = B.tArray THEN
			IF ftype.base # B.byteType THEN ArrayLen(y, par.left)
			ELSE SizeOf(y, par.left)
			END; Load(y)
		ELSIF (par.left IS B.Par) & par.left(B.Par).varpar THEN
			MakeItem0(y, par.left); TypeTag(y); Load(y)
		ELSE TypeDesc(y, par.left.type); LoadAdr(y)
		END;
		IF n > 2 THEN
			SetRm_regI(reg_SP, n*8+8); EmitRegRm(MOV, y.r, 8); FreeReg(y.r)
		ELSIF ParReg(n+1) # y.r THEN ASSERT(FALSE)
		END
	END
END Parameter;

PROCEDURE Call(VAR x: Item; node: B.Node);
	VAR y: Item; oldStat: MakeItemState; oldAlloc, oldXAlloc: SET;
		procType: B.Type; blk: Block;
BEGIN
	procType := node.left.type; ResetMkItmStat2(oldStat);
	IF procType.base = NIL THEN allocReg := {}; allocXReg := {} END;
	oldAlloc := allocReg; oldXAlloc := allocXReg;
	IF curProc.homeSpace < 32 THEN curProc.homeSpace := 32 END;
	IF curProc.homeSpace < procType.parblksize THEN
		curProc.homeSpace := procType.parblksize
	END;
	
	IF node.left IS B.Proc THEN MakeItem0(x, node.left)
	ELSE
		AvoidUsedBy(node.right); MakeItem0(x, node.left);
		Load(x); (*EmitRR(TEST, x.r, 8, x.r); Trap(ccZ, nilTrap)*)
	END;
	IF node.right # NIL THEN
		Parameter(node.right(B.Node), procType.fields, 0)
	END;
	IF x.mode = mReg THEN SetRm_reg(x.r); EmitRm(CALL, 4)
	ELSIF x.ref THEN SetRmOperand(x); EmitRm(CALL, 4)
	ELSE
		blk := curBlk; OpenBlock(ccAlways); blk.call := TRUE;
		blk.jDst := FindProcBlk(node.left)
	END;
	MkItmStat := oldStat; allocReg := oldAlloc; allocXReg := oldXAlloc;
	IF procType.base # NIL THEN
		IF procType.base.form # B.tReal THEN
			x.mode := mReg; x.r := reg_A; SetAlloc(reg_A)
		ELSE x.mode := mXReg; x.r := 0; SetAllocX(0)
		END;
		x.ref := FALSE
	ELSE x.mode := mNothing
	END
END Call;

PROCEDURE StdFunc(VAR x: Item; node: B.Node);
	VAR id, op: INTEGER; r: BYTE;
		oldStat: MakeItemState; y: Item; blk: Block;
		par1, par2: B.Node; obj1, obj2: B.Object;
BEGIN id := node.op; obj1 := node.left; obj2 := node.right;
	IF id = S.sfABS THEN MakeItem0(x, obj1); Load(x);
		IF x.type.form = B.tInt THEN EmitRR(TEST, x.r, 8, x.r);
			blk := curBlk; OpenBlock(ccGE); EmitR(NEG, x.r, 8);
			OpenBlock(255); blk.jDst := curBlk; FJump0(blk); MergeFrom(blk)
		ELSIF x.type = B.realType THEN
			r := AllocReg2({}); SetRm_reg(r);
			EmitXmmRm(SseMOVDd, x.r, 8); EmitRI(BTRi, r, 8, 63);
			SetRm_reg(r); EmitXmmRm(SseMOVD, x.r, 8); FreeReg(r)
		END
	ELSIF id = S.sfODD THEN
		ResetMkItmStat2(oldStat); MakeItem0(x, obj1); Load(x);
		EmitRI(ANDi, x.r, 4, 1); FreeReg(x.r); SetCond(x, ccNZ);
		MkItmStat := oldStat
	ELSIF id = S.sfLEN THEN (* x is open array *)
		ArrayLen(x, obj1); Load(x)
	ELSIF (id >= S.sfLSL) & (id <= S.sfROR) THEN
		oldStat := MkItmStat; AvoidUsedBy(obj2); SetAvoid(reg_C);
		MakeItem0(x, obj1); Load(x); ResetMkItmStat;
		SetBestReg(reg_C); MakeItem0(y, obj2); Load(y);
		IF y.r # reg_C THEN RelocReg(y.r, reg_C) END;
		IF id = S.sfLSL THEN op := SHLcl
		ELSIF id = S.sfROR THEN op := RORcl
		ELSIF id = S.sfASR THEN op := SARcl
		END;
		SetRm_reg(x.r); EmitRm(op, 8); FreeReg(reg_C); MkItmStat := oldStat
	ELSIF id = S.sfFLOOR THEN
		oldStat := MkItmStat; r := AllocReg(); ResetMkItmStat;
		MakeItem0(x, obj1); Load(x); EmitRI(SUBi, reg_SP, 8, 8);
		SetRm_regI(reg_SP, 0); EmitRm(STMXCSR, 4);
		EmitRmImm(BTSi, 4, 13); EmitRm(LDMXCSR, 4);
		SetRm_reg(x.r); EmitXmmRm(CVTSD2SI, r, 8); FreeXReg(x.r);	
		SetRm_regI(reg_SP, 0); EmitRmImm(BTSi, 4, 13);
		EmitRm(LDMXCSR, 4); EmitRI(ADDi, reg_SP, 8, 8);
		x.mode := mReg; x.r := r; MkItmStat := oldStat
	ELSIF id = S.sfFLT THEN
		oldStat := MkItmStat; r := AllocXReg(); ResetMkItmStat;
		MakeItem0(x, obj1); Load(x); SetRm_reg(x.r);
		EmitXmmRm(CVTSI2SD, r, 8); FreeReg(x.r);
		x.mode := mXReg; x.r := r; MkItmStat := oldStat
	ELSIF id = S.sfORD THEN MakeItem0(x, obj1); Load(x)
	ELSIF id = S.sfCHR THEN MakeItem0(x, obj1);
		IF x.type = B.byteType THEN Load(x)
		ELSIF x.mode IN {mReg, mRegI} THEN
			SetRmOperand(x); EmitMOVZX(x.r, 2); x.mode := mReg
		ELSE SetRmOperand(x); r := AllocReg();
			EmitMOVZX(r, 2); x.mode := mReg; x.r := r
		END
	ELSIF id = S.sfADR THEN MakeItem0(x, obj1); LoadAdr(x)
	ELSIF id = S.sfBIT THEN
		ResetMkItmStat2(oldStat); AvoidUsedBy(obj2);
		MakeItem0(x, obj1); Load(x); ResetMkItmStat;
		MakeItem0(y, obj2); SetRm_regI(x.r, 0);
		IF y.mode = mImm THEN EmitRmImm(BTi, 8, y.a MOD 64)
		ELSE Load(y); EmitRegRm(BT, y.r, 8); FreeReg(y.r)
		END;
		FreeReg(x.r); SetCond(x, ccC)
	ELSIF id = S.sfVAL THEN
		IF node.type = obj1.type THEN MakeItem0(x, obj1); Load(x)
		ELSIF (node.type = B.realType) & (obj1.type.form # B.tReal) THEN
			oldStat := MkItmStat; r := AllocXReg(); ResetMkItmStat;
			MakeItem0(x, obj1); Load(x); SetRm_reg(x.r);
			EmitXmmRm(SseMOVD, r, 8); FreeReg(x.r);
			x.mode := mXReg; x.r := r; MkItmStat := oldStat
		ELSIF (obj1.type = B.realType) & (node.type.form # B.tReal) THEN
			oldStat := MkItmStat; r := AllocReg(); ResetMkItmStat;
			MakeItem0(x, obj1); Load(x);
			SetRm_reg(r); EmitXmmRm(SseMOVDd, x.r, 8);
			FreeXReg(x.r); x.mode := mReg; x.r := r; MkItmStat := oldStat;
			IF node.type.size < 8 THEN
				SetRm_reg(x.r); EmitMOVZX(x.r, node.type.size)
			END
		ELSIF (obj1.type.form # B.tReal) & (node.type.form # B.tReal) THEN
			MakeItem0(x, obj1);
			IF node.type.size >= obj1.type.size THEN Load(x)
			ELSIF x.mode IN {mReg, mRegI} THEN SetRmOperand(x);
				EmitMOVZX(x.r, obj1.type.size); x.mode := mReg
			ELSE r := AllocReg(); SetRmOperand(x);
				EmitMOVZX(r, obj1.type.size); x.mode := mReg; x.r := r
			END
		ELSE ASSERT(FALSE)
		END
	END
END StdFunc;

PROCEDURE Becomes(node: B.Node);
	VAR x, y, z: Item; cx, rsize: INTEGER;
		orgBlk, blk2: Block;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	IF ~(node.left.type.form IN {B.tArray, B.tRec}) THEN
		AvoidUsedBy(node.right); MakeItem0(x, node.left); ResetMkItmStat;
		MakeItem0(y, node.right); Load(y); RefToRegI(x); SetRmOperand(x);
		IF y.type = B.realType THEN EmitXmmRm(MOVSDd, y.r, 4)
		ELSE EmitRegRm(MOV, y.r, x.type.size)
		END
	ELSE
		AvoidUsedBy(node.right); SetAvoid(reg_SI); SetBestReg(reg_DI);
		MakeItem0(x, node.left); LoadAdr(x); ResetMkItmStat;
		SetBestReg(reg_SI); MakeItem0(y, node.right); LoadAdr(y);
		IF y.r # reg_SI THEN RelocReg(y.r, reg_SI) END;
		IF x.r # reg_DI THEN RelocReg(x.r, reg_DI) END;
		IF y.type = B.strType THEN cx := y.strlen * 2;
			IF cx MOD 8 = 0 THEN cx := cx DIV 8; rsize := 8
			ELSIF cx MOD 4 = 0 THEN cx := cx DIV 4; rsize := 4
			ELSE cx := cx DIV 2; rsize := 2
			END;
			SetAlloc(reg_C); MoveRI(reg_C, 4, cx); EmitRep(MOVSrep, rsize, 1)
		ELSIF B.IsStr(x.type) THEN
			SetAlloc(reg_A); SetAlloc(reg_C);
			EmitRR(XOR, reg_C, 4, reg_C); EmitRR(XOR, reg_A, 4, reg_A);
			orgBlk := curBlk; OpenBlock(255); EmitRI(ADDi, reg_C, 8, 1);
			ArrayLen(z, node.left); Load(z); EmitRR(CMPd, reg_C, 8, z.r);
			Trap(ccA, stringTrap); FreeReg(z.r);
			ArrayLen(z, node.right); Load(z); EmitRR(CMPd, reg_C, 8, z.r);
			Trap(ccA, stringTrap); FreeReg(z.r);
			
			EmitBare(LODSW); EmitBare(STOSW); EmitRR(TEST, reg_A, 4, reg_A);
			blk2 := curBlk; OpenBlock(ccNZ); blk2.jDst := blk2; BJump(blk2);
		ELSIF B.IsOpenArray(y.type) THEN
			SetAlloc(reg_C); ArrayLen(z, node.right); SetRmOperand(z);
			EmitRegRm(MOVd, reg_C, 8); EmitRI(CMPi, reg_C, 8, x.type.len);
			rsize := y.type.base.align; cx := y.type.base.size DIV rsize;
			Trap(ccA, arrayLenTrap); EmitRI(IMULi, reg_C, 8, cx);
			EmitRep(MOVSrep, rsize, 1)
		ELSE cx := x.type.size;
			IF cx MOD 8 = 0 THEN cx := cx DIV 8; rsize := 8
			ELSIF cx MOD 4 = 0 THEN cx := cx DIV 4; rsize := 4
			ELSIF ODD(cx) THEN rsize := 1
			ELSE cx := cx DIV 2; rsize := 2
			END;
			SetAlloc(reg_C); MoveRI(reg_C, 4, cx);
			EmitRep(MOVSrep, rsize, 1)
		END
	END;
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END Becomes;

PROCEDURE If(node: B.Node);
	VAR x, y: Item; blk: Block; then: B.Node;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	LoadCond(x, node.left); curBlk.link := x.aLink; x.aLink := curBlk;
	OpenBlock(negated(x.c)); FixLink(x.bLink); then := node.right(B.Node);
	MakeItem0(y, then.left); blk := curBlk; OpenBlock(ccAlways);
	FixLink(x.aLink); IF then.right # NIL THEN MakeItem0(y, then.right) END;
	OpenBlock(255); blk.jDst := curBlk; FJump(blk);
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END If;

PROCEDURE While(node: B.Node; first: Block);
	VAR x, y: Item; do: B.Node; blk: Block;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	IF first = NIL THEN
		IF CodeLen() = 0 THEN first := curBlk
		ELSE OpenBlock(255); first := curBlk
		END
	END;
	LoadCond(x, node.left);
	curBlk.link := x.aLink; x.aLink := curBlk; OpenBlock(negated(x.c));
	FixLink(x.bLink); do := node.right(B.Node); MakeItem0(y, do.left);
	blk := curBlk; OpenBlock(ccAlways);
	blk.jDst := first; BJump(blk); FixLink(x.aLink);
	IF do.right # NIL THEN While(do.right(B.Node), first) END;
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END While;

PROCEDURE Repeat(node: B.Node);
	VAR x, y: Item; first, blk: Block;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	IF CodeLen() = 0 THEN first := curBlk
	ELSE OpenBlock(255); first := curBlk
	END;
	MakeItem0(x, node.left); LoadCond(y, node.right);
	curBlk.link := y.aLink; y.aLink := curBlk; OpenBlock(negated(y.c));
	FixLinkWith(y.aLink, first); FixLink(y.bLink);
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END Repeat;

PROCEDURE For(node: B.Node);
	VAR i, b, e: Item; control, beg, end: B.Node; by: B.Object;
		inc, op, opI: INTEGER; r, cc: BYTE; blk, blk2: Block;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	control := node.left(B.Node); beg := control.right(B.Node);
	end := beg.right(B.Node); by := end.right;
	
	ASSERT(control.left IS B.Var); MakeItem0(b, beg.left);
	IF (b.mode = mImm) & SmallConst(b.a) THEN
		MakeItem0(i, control.left); RefToRegI(i);
		SetRmOperand(i); EmitRmImm(MOVi, 8, b.a); FreeReg2(i)
	ELSE Load(b); MakeItem0(i, control.left); RefToRegI(i);
		SetRmOperand(i); EmitRegRm(MOV, b.r, 8); FreeReg2(i); FreeReg(b.r)
	END;	
	IF by = NIL THEN inc := 1 ELSE inc := by(B.Node).left(B.Const).val END;
	IF inc >= 0 THEN cc := ccG; opI := ADDi; op := ADD
	ELSE cc := ccL; opI := SUBi; op := SUB
	END;
	
	OpenBlock(255); blk := curBlk;
	AvoidUsedBy(end.left); MakeItem0(i, control.left); RefToRegI(i);
	IF (e.mode = mImm) & SmallConst(e.a) THEN
		SetRmOperand(i); EmitRmImm(CMPi, 8, e.a)
	ELSE
		ResetMkItmStat; MakeItem0(e, end.left); Load(e);
		SetRmOperand(i); EmitRegRm(CMP, e.r, 8); FreeReg(e.r)
	END;
	FreeReg2(i); OpenBlock(cc);
	
	MakeItem0(i, node.right); (* Statement sequence *)
	MakeItem0(i, control.left); RefToRegI(i); SetRmOperand(i);
	IF inc = 1 THEN EmitRm(INC_, 8) ELSIF inc = -1 THEN EmitRm(DEC_, 8)
	ELSIF SmallConst(inc) THEN EmitRmImm(opI, 8, inc)
	ELSE r := AllocReg(); LoadImm(r, 8, inc); EmitRegRm(op, r, 8)
	END;
	blk2 := curBlk; OpenBlock(ccAlways); blk2.jDst := blk;
	blk.jDst := curBlk; BJump(blk2); FJump(blk);
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END For;

PROCEDURE StdProc(node: B.Node);
	VAR id, size: INTEGER; r, r2: BYTE;
		x, y, z: Item; blk1, blk2: Block; obj1, obj2, obj3: B.Object;
BEGIN ResetMkItmStat; allocReg := {}; allocXReg := {};
	id := node.op; obj1 := node.left; obj2 := node.right;
	IF id = S.spINC THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x);
		IF obj2 = NIL THEN SetRmOperand(x); EmitRm(INC_, x.type.size)
		ELSE SetRmOperand(x); r := AllocReg();
			EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
			MakeItem0(y, obj2); Load(y); EmitRR(ADDd, r, x.type.size, y.r);
			SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
		END
	ELSIF id = S.spDEC THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x);
		IF obj2 = NIL THEN SetRmOperand(x); EmitRm(DEC_, x.type.size)
		ELSE SetRmOperand(x); r := AllocReg();
			EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
			MakeItem0(y, obj2); Load(y); EmitRR(SUBd, r, x.type.size, y.r);
			SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
		END
	ELSIF id = S.spINCL THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); SetRmOperand(x);
		r := AllocReg(); EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRR(BTS, y.r, x.type.size, r);
		SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
	ELSIF id = S.spEXCL THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); SetRmOperand(x);
		r := AllocReg(); EmitRegRm(MOVd, r, x.type.size); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRR(BTR, y.r, x.type.size, r);
		SetRmOperand(x); EmitRegRm(MOV, r, x.type.size)
	ELSIF id = S.spNEW THEN
		SetAlloc(reg_C); SetAlloc(reg_D); SetAlloc(reg_R8); SetAlloc(reg_A);
		SetRm_regI(reg_B, B.HeapHandle); EmitRegRm(MOVd, reg_C, 8);
		MoveRI(reg_D, 4, 8); size := obj1.type.base.size + 15 DIV 16 * 16;
		MoveRI(reg_R8, 4, size + 16); SetRm_regI(reg_B, B.HeapAlloc);
		EmitRm(CALL, 4); EmitRI(ADDi, reg_A, 8, 16); FreeReg(reg_C);
		FreeReg(reg_D); FreeReg(reg_R8);
		
		TypeDesc(y, obj1.type.base); LoadAdr(y);
		SetRm_regI(reg_A, -8); EmitRegRm(MOV, y.r, 8); FreeReg2(y); 
		MakeItem0(x, obj1); RefToRegI(x); SetRmOperand(x);
		EmitRegRm(MOV, reg_A, 8); FreeReg2(x);
		IF curProc.homeSpace < 32 THEN curProc.homeSpace := 32 END
	ELSIF id = S.spASSERT THEN
		LoadCond(x, obj1); curBlk.link := x.bLink;
		x.bLink := curBlk; OpenBlock(x.c); FixLink(x.aLink);
		MoveRI(0, 1, assertTrap); MoveRI(1, 4, sPos); EmitBare(INT3);
		OpenBlock(255); FixLink(x.bLink)
	ELSIF id = S.spPACK THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); r := AllocReg();
		SetRmOperand(x); EmitRegRm(MOVd, r, 8); ResetMkItmStat;
		MakeItem0(y, obj2); Load(y); EmitRI(SHLi, y.r, 8, 52);
		EmitRR(ADDd, r, 8, y.r); SetRmOperand(x); EmitRegRm(MOV, r, 8)
	ELSIF id = S.spUNPK THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); RefToRegI(x); r := AllocReg();
		SetRmOperand(x); EmitRegRm(MOVd, r, 8); ResetMkItmStat;
		MakeItem0(y, obj2); RefToRegI(y); r2 := AllocReg();
		EmitRR(MOVd, r2, 8, r); EmitRI(SHRi, r2, 8, 52);
		EmitRI(SUBi, r2, 4, 1023); SetRmOperand(y);
		EmitRegRm(MOV, r2, 8); EmitRI(SHLi, r2, 8, 52);
		EmitRR(SUBd, r, 8, r2); SetRmOperand(x); EmitRegRm(MOV, r, 8)
	ELSIF id = S.spGET THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); Load(x);
		SetRm_regI(x.r, 0); EmitRegRm(MOVd, x.r, obj2.type.size);
		ResetMkItmStat; MakeItem0(y, obj2); RefToRegI(y); SetRmOperand(y);
		EmitRegRm(MOV, x.r, y.type.size)
	ELSIF id = S.spPUT THEN
		AvoidUsedBy(obj2); MakeItem0(x, obj1); Load(x);
		ResetMkItmStat; MakeItem0(y, obj2); Load(y); SetRm_regI(x.r, 0);
		EmitRegRm(MOV, y.r, y.type.size)
	ELSIF id = S.spCOPY THEN
		obj3 := obj2(B.Node).right; obj2 := obj2(B.Node).left;
		AvoidUsedBy(obj2); AvoidUsedBy(obj3); SetAvoid(reg_DI);
		SetAvoid(reg_C); SetBestReg(reg_SI); MakeItem0(x, obj1); Load(x);
		ResetMkItmStat; AvoidUsedBy(obj3); SetAvoid(reg_C);
		SetBestReg(reg_DI); MakeItem0(y, obj2); Load(y);
		ResetMkItmStat; SetBestReg(reg_C); MakeItem0(z, obj3); size := 1;
		IF z.mode = mImm THEN
			IF z.a <= 0 THEN z.a := 0; size := 0
			ELSIF z.a MOD 8 = 0 THEN z.a := z.a DIV 8; size := 8
			ELSIF z.a MOD 4 = 0 THEN z.a := z.a DIV 4; size := 4
			ELSIF z.a MOD 2 = 0 THEN z.a := z.a DIV 2; size := 2
			END
		END;
		IF size > 0 THEN Load(z);
			IF z.r # reg_C THEN RelocReg(z.r, reg_C) END;
			IF y.r # reg_DI THEN RelocReg(y.r, reg_DI) END;
			IF x.r # reg_SI THEN RelocReg(x.r, reg_SI) END;
			EmitRep(MOVSrep, size, 1)
		END
	ELSIF id = S.spINT3 THEN
		EmitBare(INT3)
	ELSE ASSERT(FALSE)
	END;
	ResetMkItmStat; allocReg := {}; allocXReg := {}
END StdProc;

PROCEDURE OpImm(VAR x: Item; node: B.Node);
	VAR imm, cond: INTEGER; oldStat: MakeItemState;
BEGIN imm := node.right(B.Const).val;
	IF node.op = S.plus THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(ADDi, x.r, 8, imm)
		ELSIF x.type = B.setType THEN EmitRI(ORi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.minus THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(SUBi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.times THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type.form = B.tInt THEN EmitRI(IMULi, x.r, 8, imm)
		ELSIF x.type = B.setType THEN EmitRI(ANDi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF node.op = S.rdiv THEN
		MakeItem0(x, node.left); Load(x);
		IF x.type = B.setType THEN EmitRI(XORi, x.r, 8, imm)
		ELSE ASSERT(FALSE)
		END
	ELSIF (node.op >= S.eql) & (node.op <= S.geq) THEN
		oldStat := MkItmStat; ResetMkItmStat;
		MakeItem0(x, node.left); RefToRegI(x); MkItmStat := oldStat;
		SetRmOperand(x); EmitRmImm(CMPi, x.type.size, imm);
		IF x.type.form = B.tInt THEN cond := IntOpToCc(node.op)
		ELSE cond := OpToCc(node.op)
		END;
		FreeReg2(x); SetCond(x, cond)
	ELSIF node.op = S.becomes THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x);
		SetRmOperand(x); EmitRmImm(MOVi, x.type.size, imm);
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSIF (node.op >= S.sfLSL) & (node.op <= S.sfROR) THEN
		MakeItem0(x, node.left); Load(x);
		IF imm = 0 THEN (* do nothing *)
		ELSIF imm = 1 THEN
			IF node.op = B.sfLSL THEN EmitR(SHL1, x.r, 8)
			ELSIF node.op = B.sfASR THEN EmitR(SAR1, x.r, 8)
			ELSE EmitR(ROR1, x.r, 8)
			END
		ELSIF node.op = B.sfLSL THEN EmitRI(SHLi, x.r, 8, imm MOD 64)
		ELSIF node.op = B.sfASR THEN EmitRI(SARi, x.r, 8, imm MOD 64)
		ELSE EmitRI(RORi, x.r, 8, imm MOD 64)
		END
	ELSIF (node.op = S.spINC) OR (node.op = S.spDEC) THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x); SetRmOperand(x);
		IF node.op = S.spINC THEN EmitRmImm(ADDi, x.type.size, imm)
		ELSE EmitRmImm(SUBi, x.type.size, imm)
		END;
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSIF (node.op = S.spINCL) OR (node.op = S.spEXCL) THEN
		ResetMkItmStat; allocReg := {}; allocXReg := {};
		MakeItem0(x, node.left); RefToRegI(x); SetRmOperand(x);
		IF node.op = S.spINCL THEN EmitRmImm(BTSi, x.type.size, imm MOD 64)
		ELSE EmitRmImm(BTRi, x.type.size, imm MOD 64)
		END;
		ResetMkItmStat; allocReg := {}; allocXReg := {}
	ELSE ASSERT(FALSE)
	END
END OpImm;

PROCEDURE MakeItem(VAR x: Item; obj: B.Object);
	VAR objv: B.Var; node: B.Node; size, form: INTEGER;
		flag: BOOLEAN; const: INTEGER;
BEGIN
	x.type := obj.type; x.ref := FALSE; x.a := 0; x.b := 0; x.c := 0;
	IF obj IS B.Const THEN x.mode := mImm; x.a := obj(B.Const).val
	ELSIF obj IS B.Var THEN
		objv := obj(B.Var); x.a := objv.adr; form := objv.type.form;
		IF objv.lev <= 0 THEN x.mode := mBX ELSE x.mode := mBP END;
		IF objv.lev < 0 THEN x.ref := TRUE END;
		IF objv IS B.Str THEN x.mode := mBX; x.strlen := objv(B.Str).len
		ELSIF objv IS B.Par THEN
			x.ref := objv(B.Par).varpar OR (form = B.tArray) OR (form = B.tRec)
		END
	ELSIF obj IS B.Proc THEN x.mode := mProc;
		IF obj(B.Proc).lev < 0 THEN
			x.ref := TRUE; x.a := obj(B.Proc).adr
		ELSE x.aLink := FindProcBlk(obj)
		END
	ELSIF obj.class = B.cType THEN ASSERT(FALSE)
	ELSIF obj IS B.Node THEN
		node := obj(B.Node); sPos := node.srcPos; x.mode := mNothing;
		IF (node.right # NIL) & (node.right IS B.Const) THEN
			const := node.right(B.Const).val;
			flag := (node.op >= S.sfLSL) & (node.op <= S.sfROR)
				OR (node.op = S.spINCL) OR (node.op = S.spEXCL)
				OR SmallConst(const) & (node.right.type.form # B.tReal)
					& ((node.right.type # B.setType) OR (node.op = S.minus))
					& (node.op # S.div) & (node.op # S.mod)
					& (node.op # S.and) & (node.op # S.or)
					& (node.op # S.in) & (node.op # S.lbrak)
					& ((node.op # S.becomes) OR (const # 0))
		ELSE flag := FALSE
		END;
		IF flag THEN OpImm(x, node)
		ELSIF node.op = S.plus THEN Add(x, node)
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
		ELSIF (node.op >= S.sfABS) & (node.op <= S.sfVAL) THEN StdFunc(x, node)
		ELSIF node.op = S.becomes THEN Becomes(node)
		ELSIF node.op = S.if THEN If(node)
		ELSIF node.op = S.while THEN While(node, NIL)
		ELSIF node.op = S.repeat THEN Repeat(node)
		ELSIF node.op = S.for THEN For(node)
		ELSIF node.op = S.case THEN
			Sys.Console_WriteStr('CASE not supported yet'); ASSERT(FALSE)
		ELSIF node.op = S.semicolon THEN
			IF node.left # NIL THEN MakeItem(x, node.left) END;
			IF node.right # NIL THEN MakeItem(x, node.right) END
		ELSIF (node.op >= S.spINC) & (node.op <= S.spINT3) THEN StdProc(node)
		ELSE ASSERT(FALSE)
		END;
		IF x.mode # mNothing THEN x.type := node.type;
			IF (x.mode IN {mReg, mRegI}) & (x.r IN MkItmStat.avoid) THEN
				RelocReg(x.r, AllocReg())
			ELSIF (x.mode = mXReg) & (x.r IN MkItmStat.xAvoid) THEN
				RelocReg(x.r, AllocXReg())
			END
		END
	END
END MakeItem;

PROCEDURE Debug;
	VAR file: Sys.File; b: BYTE; rider: Sys.MemFileRider; blk: Block;
		i: INTEGER;
BEGIN
	Sys.Rewrite(file, 'Test.dat');
	blk := curProc.blk;
	WHILE blk # NIL DO
		Sys.SetMemFile(rider, blk.code, 0);
		REPEAT Sys.ReadMemFile(rider, b);
			IF ~rider.eof THEN Sys.Write1(file, b) END
		UNTIL rider.eof;
		IF ~blk.finished THEN i := 0;
			IF blk.call THEN
				WHILE i < 5 DO Sys.Write1(file, 90H); INC(i) END
			ELSE WHILE i < 7 DO Sys.Write1(file, 90H); INC(i) END
			END
		END;
		blk := blk.next
	END;
	Sys.Close(file)
END Debug;

PROCEDURE SetPtrToNil(adr: INTEGER; type: B.Type);
	VAR i: INTEGER; fld: B.Ident;
BEGIN
	IF type.form = B.tPtr THEN SetRm_regI(reg_BP, adr); EmitRmImm(MOVi, 8, 0)
	ELSIF type.form = B.tArray THEN
		FOR i := 0 TO type.len-1 DO
			SetPtrToNil(adr+i*type.base.size, type.base)
		END
	ELSIF type.form = B.tRec THEN
		fld := type.fields; ASSERT(fld # NIL);
		REPEAT
			IF fld.obj.type.nptr > 0 THEN
				SetPtrToNil(adr+fld.obj(B.Field).off, fld.obj.type)
			END;
			fld := fld.next
		UNTIL fld = NIL
	ELSE ASSERT(FALSE)
	END
END SetPtrToNil;

PROCEDURE Procedure;
	VAR locblksize, nSave, nSaveX, n, i, j, off, procCodeSize: INTEGER;
		r: BYTE; x: Item; blk, src, dst, epilog: Block; obj: B.Proc;
		param, ident: B.Ident; pType: B.Type;
BEGIN
	curProc.homeSpace := 0; curProc.stack := 0;
	curProc.usedReg := {}; curProc.usedXReg := {};
	IF curProc.export THEN INCL(curProc.usedReg, reg_B) END;
	
	obj := curProc.obj; obj.adr := pc;
	curBlk := curProc.blk; epilog := curBlk;
	locblksize := (obj.locblksize + 7) DIV 8 * 8;
	curProc.stack := locblksize; curProc.pStk := locblksize;
	
	IF (obj.statseq # NIL) OR (obj.return # NIL) THEN
		OpenBlock(255); ident := obj.decl;
		WHILE ident # NIL DO
			IF (ident.obj IS B.Var) & ~(ident.obj IS B.Par) THEN
				IF ident.obj.type.nptr > 0 THEN
					SetPtrToNil(ident.obj(B.Var).adr, ident.obj.type)
				END
			END;
			ident := ident.next
		END;
		IF obj.statseq # NIL THEN MakeItem(x, obj.statseq) END;
		IF obj.return # NIL THEN
			ResetMkItmStat; MakeItem(x, obj.return); Load(x);
			IF x.r # 0 THEN
				IF x.mode = mReg THEN RelocReg(x.r, 0)
				ELSE RelocXReg(x.r, 0)
				END
			END
		END;
		locblksize := curProc.stack; epilog := curBlk; curBlk := curProc.blk
	END;
	
	PushR(reg_BP); EmitRR(MOVd, reg_BP, 8, reg_SP);
	nSave := 0; nSaveX := 0; r := 0;
	WHILE r < 16 DO
		IF r IN curProc.usedReg*{3 .. 7, 12 .. 15} THEN INC(nSave) END;
		IF r IN curProc.usedXReg*{6 .. 15} THEN INC(nSaveX) END;
		INC(r)
	END;
	n := ((locblksize + nSave*8 + 8) DIV 16 + nSaveX) * 16;
	curProc.homeSpace := (curProc.homeSpace + 15) DIV 16 * 16;
	IF n+curProc.homeSpace # 0 THEN
		EmitRI(SUBi, reg_SP, 8, n+curProc.homeSpace)
	END;
	(*EmitRI(SUBi, reg_SP, 8, n); EmitRI(SUBi, reg_SP, 8, curProc.homeSpace);*)
	
	r := 0; i := 0; j := 0;
	WHILE r < 16 DO
		IF r IN curProc.usedReg*{3 .. 7, 12 .. 15} THEN
			SetRm_regI(reg_BP, -n+nSaveX*16 + i*8);
			EmitRegRm(MOV, r, 8); INC(i)
		END;
		IF r IN curProc.usedXReg*{6 .. 15} THEN
			SetRm_regI(reg_BP, -n + j*16);
			EmitXmmRm(MOVAPSd, r, 4); INC(j)
		END;
		INC(r)
	END;
	IF curProc.export THEN
		SetRm_RIP(-pc-CodeLen()-7); EmitRegRm(LEA, reg_B, 8)
	END;
	
	IF obj.type # NIL THEN
		param := obj.type.fields; i := 0;
		WHILE (param # NIL) & (i < 4) DO
			pType := param.obj.type; SetRm_regI(reg_BP, 16+i*8);
			IF pType.form # B.tReal THEN
				EmitRegRm(MOV, ParReg(i), 8);
				IF (pType.form = B.tRec) & (param.obj(B.Par).varpar)
				OR B.IsOpenArray(pType) THEN INC(i);
					IF i < 4 THEN
						SetRm_regI(reg_BP, 16+i*8);
						EmitRegRm(MOV, ParReg(i), 8)
					END
				END
			ELSE EmitXmmRm(MOVSDd, i, 4)
			END;
			INC(i); param := param.next
		END
	END;
	
	curBlk := epilog; r := 0; i := 0; j := 0;
	WHILE r < 16 DO
		IF r IN curProc.usedReg*{3 .. 7, 12 .. 15} THEN
			SetRm_regI(reg_BP, -n+nSaveX*16 + i*8);
			EmitRegRm(MOVd, r, 8); INC(i)
		END;
		IF r IN curProc.usedXReg*{6 .. 15} THEN
			SetRm_regI(reg_BP, -n + j*16);
			EmitXmmRm(MOVAPS, r, 4); INC(j)
		END;
		INC(r)
	END;
	EmitBare(LEAVE); EmitBare(RET);
	
	src := curProc.blk;
	WHILE src # NIL DO
		IF (src.jmpOff # 0) & ~src.call & ~src.load THEN
			dst := src.jDst; off := 0;
			IF src.no < dst.no THEN blk := src.next;
				WHILE blk # dst DO
					INC(off, CodeLen1(blk)); blk := blk.next
				END
			ELSE DEC(off, CodeLen0(src)); blk := dst;
				WHILE blk # src DO
					DEC(off, CodeLen1(blk)); blk := blk.next
				END
			END;
			IF off # src.jmpOff THEN curBlk := src;
				(*Sys.Console_WriteInt(off); Sys.Console_Write(' ');
				Sys.Console_WriteInt(src.jmpOff); Sys.Console_WriteLn;*)
				IF off < 0 THEN ASSERT(off > src.jmpOff)
				ELSIF off > 0 THEN ASSERT(off < src.jmpOff)
				END;
				IF (src.jmpOff >= -128) & (src.jmpOff <= 127) THEN
					SetCodePos(CodeLen()-2);
					IF src.jc = ccAlways THEN Branch1(off)
					ELSE CondBranch1(src.jc, off)
					END
				ELSIF src.jc = ccAlways THEN
					SetCodePos(CodeLen()-5); Branch(off)
				ELSE SetCodePos(CodeLen()-6); CondBranch(src.jc, off)
				END
			END
		END;
		src := src.next
	END;

	src := curProc.blk; procCodeSize := 0;
	WHILE src # NIL DO
		IF src.finished THEN blk := src.next;
			WHILE (blk # NIL) & blk.finished DO
				MergeNextBlock(src); blk := src.next
			END;
			src.jDst := NIL; src.link := NIL
		END;
		INC(procCodeSize, CodeLen1(src)); src := src.next
	END;
	IF procCodeSize MOD 16 # 0 THEN blk := curProc.blk;
		WHILE blk.next # NIL DO blk := blk.next END; curBlk := blk;
		WHILE procCodeSize MOD 16 # 0 DO INC(procCodeSize); Put1(90H) END
	END;
	INC(pc, procCodeSize)
END Procedure;

PROCEDURE ModuleInit(statseq: B.Node);
	VAR obj: B.Proc;
BEGIN curProc := procList;
	IF curProc = NIL THEN NEW(procList); curProc := procList
	ELSE WHILE curProc.next # NIL DO curProc := curProc.next END;
		NEW(curProc.next); curProc := curProc.next
	END;
	obj := B.NewProc(); obj.statseq := statseq; obj.locblksize := 0;
	curProc.obj := obj; NewBlock(curProc.blk);
	Linker.entry := pc; Procedure
END ModuleInit;

PROCEDURE MergeAllProcedure;
	VAR proc: Proc; blk, src, dst: Block; off: INTEGER;
BEGIN proc := procList;
	WHILE proc # NIL DO blk := proc.blk; blk.proc := proc;
		WHILE blk.next # NIL DO
			blk.next.proc := proc; blk.next.no := blk.no+1; blk := blk.next
		END;
		IF proc.next # NIL THEN
			blk.next := proc.next.blk; blk.next.no := blk.no+1
		END;
		proc := proc.next
	END;
	src := procList.blk;
	WHILE src # NIL DO
		IF ~src.finished THEN
			IF ~src.call THEN ASSERT(src.load) END; dst := src.jDst; off := 0;
			IF src.no < dst.no THEN blk := src.next;
				WHILE blk # dst DO INC(off, CodeLen1(blk)); blk := blk.next END
			ELSE DEC(off, CodeLen1(src)); blk := dst;
				WHILE blk # src DO DEC(off, CodeLen1(blk)); blk := blk.next END
			END;
			IF off # 0 THEN curBlk := src;
				IF src.call THEN CallNear(off)
				ELSE SetRm_RIP(off); EmitRegRm(LEA, src.jc, 8)
				END
			END;
			src.finished := TRUE
		END;
		src := src.next
	END;
	MergeFrom(procList.blk)
END MergeAllProcedure;

PROCEDURE DLLInit;
	VAR i, adr, expno: INTEGER; blk: Block;
		imod: B.Module; key: B.ModuleKey; ident: B.Ident; x: B.Object;
		t: B.TypeList; tp: B.Type; 
BEGIN
	IF B.CplFlag.debug THEN EmitBare(INT3) END;
	IF ~B.CplFlag.main THEN
		EmitRI(CMPi, reg_D, 4, 1); blk := curBlk; OpenBlock(ccZ);
		EmitBare(RET); OpenBlock(255); blk.jDst := curBlk; FJump0(blk);
		MergeFrom(blk)
	END;
	
	PushR(reg_SI); PushR(reg_DI); PushR(reg_B); EmitRI(SUBi, reg_SP, 8, 32);
	SetRm_RIP(-CodeLen()-7); EmitRegRm(LEA, reg_B, 8);
	SetRm_regI(reg_B, B.GetProcessHeap); EmitRm(CALL, 4);
	SetRm_regI(reg_B, B.HeapHandle); EmitRegRm(MOV, reg_A, 8);
	
	(* Import modules, if there are any *)
	i := 0;
	WHILE i < B.modno DO imod := B.modList[i];
		SetRm_regI(reg_B, imod.adr); EmitRegRm(LEA, reg_C, 8);
		SetRm_regI(reg_B, B.LoadLibraryW.adr); EmitRm(CALL, 4);
		EmitRR(TEST, reg_A, 8, reg_A); Trap(ccZ, modkeyTrap);
		EmitRR(MOVd, reg_SI, 8, reg_A);
		
		(* Check module key *)
		key := imod.key; MoveRI(reg_A, 8, key[0]); SetRm_regI(reg_SI, 400H-16);
		EmitRegRm(CMPd, reg_A, 8); Trap(ccNZ, modkeyTrap);
		MoveRI (reg_A, 8, key[1]); SetRm_regI(reg_SI, 400H-8);
		EmitRegRm(CMPd, reg_A, 8); Trap(ccNZ, modkeyTrap);
	
		ident := imod.impList;
		WHILE ident # NIL DO x := ident.obj;
			IF x.class = B.cType THEN
				IF x.type.form = B.tRec THEN
					adr := x.type.adr; expno := x.type.expno
				ELSE adr := x.type.base.adr; expno := x.type.base.expno
				END
			ELSIF x IS B.Var THEN
				adr := x(B.Var).adr; expno := x(B.Var).expno
			ELSIF x IS B.Proc THEN
				adr := x(B.Proc).adr; expno := x(B.Proc).expno
			END;
			EmitRR(MOVd, reg_C, 8, reg_SI); MoveRI(reg_D, 4, expno);
			SetRm_regI(reg_B, B.GetProcAddress.adr); EmitRm(CALL, 4);
			SetRm_regI(reg_B, adr); EmitRegRm(MOV, reg_A, 8);
			ident := ident.next
		END;
		INC(i)
	END;
	
	(* Fill value into type descriptors *)
	t := B.recList;
	WHILE t # NIL DO
		tp := t.type; adr := tp.adr; SetRm_regI(reg_B, adr);
		IF SmallConst(tp.size) THEN EmitRmImm(MOVi, 8, tp.size)
		ELSE ASSERT(FALSE)
		END;
		WHILE tp.len >= 1 DO
			SetRm_regI(reg_B, tp.adr);
			IF tp.lev >= 0 THEN EmitRegRm(LEA, reg_A, 8)
			ELSE EmitRegRm(MOVd, reg_A, 8)
			END;
			SetRm_regI(reg_B, adr + tp.len*8);
			EmitRegRm(MOV, reg_A, 8); tp := tp.base
		END;
		t := t.next
	END;
	
	(* Call module main procedure *)
	CallNear(Linker.entry - CodeLen() - 5); Linker.entry := pc;
	
	(* Exit *)
	IF B.CplFlag.main THEN EmitRR(XOR, reg_C, 4, reg_C);
		SetRm_regI(reg_B, B.ExitProcess); EmitRm(CALL, 4)
	ELSE
		MoveRI(reg_A, 4, 1); EmitRI(ADDi, reg_SP, 8, 32);
		PopR(reg_B); PopR(reg_DI); PopR(reg_SI); EmitBare(RET)
	END
END DLLInit;

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

PROCEDURE TypeTransferConst*(type: B.Type; x: B.Object): B.Object;
	VAR val: INTEGER;
BEGIN
	IF x IS B.Str THEN val := ORD(B.strbuf[x(B.Str).bufpos])
	ELSE val := x(B.Const).val
	END;
	IF type # x.type THEN
		IF type.size = 1 THEN val := val MOD 100H
		ELSIF type.size = 2 THEN val := val MOD 10000H
		ELSIF type.size = 4 THEN val := val MOD 100000000H
		END;
		x := B.NewConst(type, val)
	END;
	RETURN x
END TypeTransferConst;
	
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
			IF op = S.plus THEN val := xval + yval
			ELSIF op = S.minus THEN val := xval - yval
			ELSIF op = S.times THEN val := xval * yval
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
			ELSIF op = S.rdiv THEN val := SYSTEM.VAL(INTEGER, r1 / r2)
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
(* Linker *)

PROCEDURE Init*(modid0: B.IdStr);
BEGIN
	modid := modid0; varSize := 0; staticSize := 128;
	procList := NIL; curProc := NIL; pc := 0;
	B.intType.size := 8; B.intType.align := 8;
	B.byteType.size := 1; B.byteType.align := 1;
	B.charType.size := 2; B.charType.align := 2;
	B.boolType.size := 1; B.boolType.align := 1;
	B.setType.size := 8; B.setType.align := 8;
	B.realType.size := 8; B.realType.align := 8;
	B.nilType.size := 8; B.nilType.align := 8;

	Linker.startTime := Sys.GetTickCount();
	Sys.Rewrite(out, tempOutputName);
END Init;

PROCEDURE Align(VAR a: INTEGER; align: INTEGER);
BEGIN
	IF a > 0 THEN a := (a + align - 1) DIV align * align
	ELSIF a < 0 THEN a := a DIV align * align
	END
END Align;

PROCEDURE Write_idata_section;
	CONST table_len = LEN(Linker.Kernel32Table);
		table_size = table_len * 8; table_rva = 40;
		name_rva = table_rva + table_size; hint_rva = name_rva + 16;
	VAR i: INTEGER;
BEGIN
	Sys.Seek(out, Linker.idata_fadr);
	
	(* Import Directory Entry - Kernel32.dll *)
	Sys.Write4(out, Linker.idata_rva + table_rva);
	Sys.Write4(out, 0);
	Sys.Write4(out, 0);
	Sys.Write4(out, Linker.idata_rva + name_rva);
	i := Linker.data_rva + Linker.data_size - table_size;
	Sys.Write4(out, i);

	Sys.Seek(out, Linker.idata_fadr + table_rva); i := 0;
	WHILE i <= table_len - 2 DO
		Linker.Kernel32Table[i] := Linker.idata_rva + hint_rva + 32 * i;
		Sys.Write8(out, Linker.Kernel32Table[i]); INC(i)
	END;
	Linker.Kernel32Table[table_len - 1] := 0;
	
	Sys.Seek(out, Linker.idata_fadr + name_rva);
	Sys.WriteAnsiStr(out, 'KERNEL32.DLL');
	
	Sys.Seek(out, Linker.idata_fadr + hint_rva + 2);
	Sys.WriteAnsiStr (out, 'ExitProcess');
	Sys.Seek(out, Linker.idata_fadr + hint_rva + (32 + 2));
	Sys.WriteAnsiStr(out, 'LoadLibraryW');
	Sys.Seek(out, Linker.idata_fadr + hint_rva + (64 + 2));
	Sys.WriteAnsiStr(out, 'GetProcAddress');
	Sys.Seek(out, Linker.idata_fadr + hint_rva + (96 + 2));
	Sys.WriteAnsiStr(out, 'GetProcessHeap');
	Sys.Seek(out, Linker.idata_fadr + hint_rva + (128 + 2));
	Sys.WriteAnsiStr(out, 'HeapAlloc');
	Sys.Seek(out, Linker.idata_fadr + hint_rva + (160 + 2));
	Sys.WriteAnsiStr(out, 'HeapFree')
END Write_idata_section;

PROCEDURE Write_pointer_offset(offset: INTEGER; type: B.Type);
	VAR ident: B.Ident; field: B.Field; size, k, ptrcnt, n: INTEGER;
BEGIN ptrcnt := type.nptr;
	IF type.form = B.tRec THEN ident := type.fields;
		IF (type.base # NIL) & (type.base.nptr # 0) THEN
			Write_pointer_offset(offset, type.base)
		END;
		WHILE ident # NIL DO field := ident.obj(B.Field);
			IF field.type.nptr > 0 THEN n := offset + field.off;
				IF field.type.form = B.tPtr THEN Sys.Write8(out, n)
				ELSE Write_pointer_offset(n, field.type)
				END
			END;
			ident := ident.next
		END
	ELSIF type.form = B.tArray THEN type := type.base;
		IF type.form = B.tPtr THEN n := offset;
			REPEAT Sys.Write8(out, n); DEC(ptrcnt); INC(n, 8)
			UNTIL ptrcnt <= 0; ASSERT(ptrcnt = 0)
		ELSE k := type.nptr; size := type.size;
			REPEAT
				Write_pointer_offset(offset, type);
				DEC(ptrcnt, k); INC(offset, size)
			UNTIL ptrcnt <= 0; ASSERT(ptrcnt = 0)
		END
	ELSE ASSERT(FALSE)
	END
END Write_pointer_offset;

PROCEDURE Write_data_section;
	VAR basefadr, i, j, n: INTEGER; b: BYTE;
		imod: B.Module; ident: B.Ident; x: B.Str; t: B.TypeList;
BEGIN
	basefadr := Linker.data_fadr + Linker.data_size;
	Sys.Seek(out, basefadr - LEN(Linker.Kernel32Table)*8); i := 0;
	WHILE i < LEN(Linker.Kernel32Table) DO
		Sys.Write8(out, Linker.Kernel32Table[i]); INC(i)
	END; i := 0;
	WHILE i < B.modno DO imod := B.modList[i];
		Sys.Seek(out, basefadr + imod.adr); j := 0;
		WHILE imod.name[j] # 0X DO
			Sys.Write2(out, ORD(imod.name[j])); INC(j)
		END;
		Sys.WriteStr(out, '.dll'); INC(i)	
	END;
	ident := B.strList;
	WHILE ident # NIL DO x := ident.obj(B.Str);
		Sys.Seek(out, basefadr + x.adr); i := 0;
		WHILE i < x.len DO
			Sys.Write2(out, ORD(B.strbuf[x.bufpos+i])); INC(i)
		END;
		ident := ident.next
	END;
	t := B.recList;
	WHILE t # NIL DO
		Sys.Seek(out, basefadr + t.type.adr + 8 + B.MaxExt*8);
		IF t.type.nptr > 0 THEN Write_pointer_offset(0, t.type) END;
		Sys.Write8(out, -1); t := t.next
	END
END Write_data_section;

PROCEDURE Write_edata_section;
	CONST dirsize = 40;
	VAR ident: B.Ident; x: B.Object; name: B.String;
		namesize, tablesize, i, rva, expno: INTEGER;
BEGIN name[0] := 0X; B.AppendStr(modid, name); namesize := 0;
	IF B.CplFlag.main THEN B.AppendStr('.exe', name)
	ELSE B.AppendStr('.dll', name)
	END;
	WHILE name[namesize] # 0X DO INC(namesize) END; INC(namesize);
	expno := B.expno; tablesize := expno * 4;

	(* Export directory *)
	Sys.Seek(out, Linker.edata_fadr + 12);
	Sys.Write4(out, Linker.edata_rva + dirsize + tablesize);
	Sys.Write4(out, 1);
	Sys.Write4(out, expno);
	Sys.Write4(out, 0);
	Sys.Write4(out, Linker.edata_rva + dirsize);
	
	(* Export address table *)
	Sys.Seek(out, Linker.edata_fadr + dirsize); ident := B.expList;
	WHILE ident # NIL DO x := ident.obj;
		IF x.class = B.cType THEN rva := x.type.adr
		ELSIF x IS B.Var THEN rva := x(B.Var).adr
		ELSIF x IS B.Proc THEN rva := x(B.Proc).adr
		END; INC(rva, Linker.code_rva);
		Sys.Write4(out, rva); ident := ident.next
	END;
		
	(* Name string *)
	Sys.WriteAnsiStr(out, name);
	
	Linker.edata_size := dirsize + tablesize + namesize;
	Linker.edata_rawsize := Linker.edata_size;
	IF Linker.edata_rawsize MOD 512 # 0 THEN
		Align(Linker.edata_rawsize, 512);
		Sys.Seek(out, Linker.edata_fadr + Linker.edata_rawsize - 1);
		Sys.Write1(out, 1)
	END
END Write_edata_section;

PROCEDURE Write_reloc_section;
BEGIN
	Sys.Seek(out, Linker.reloc_fadr);
	Sys.Write4(out, 4);
	Sys.Write4(out, 12);
	Sys.Write2(out, 0);
	Sys.Write2(out, 0)
END Write_reloc_section;

PROCEDURE Write_SectionHeader (
	name: ARRAY OF CHAR; chr, rva, rawsize, size, fileadr: INTEGER
);	
	VAR b: BYTE; i: INTEGER;	
BEGIN i := 0;
	WHILE i < 8 DO b := 0;
		IF i < LEN(name) THEN b := ORD(name[i]) END;
		Sys.Write1(out, b); INC(i)
	END;
	Sys.Write4(out, size);
	Sys.Write4(out, rva);
	Sys.Write4(out, rawsize);
	Sys.Write4(out, fileadr);
	Sys.Write4(out, 0);
	Sys.Write4(out, 0);
	Sys.Write4(out, 0);
	Sys.Write4(out, chr)
END Write_SectionHeader;

PROCEDURE Write_PEHeader;
	VAR k, nSection: INTEGER;
BEGIN
	Sys.Seek(out, 0);
	Sys.Write2(out, 5A4DH);
	Sys.Seek(out, 60);
	Sys.Write4(out, 128);
	Sys.Seek (out, 128);
	Sys.Write4(out, 4550H);
	
	Sys.Write2(out, 8664H); (* Machine = AMD64/Intel 64 *)
	IF Linker.bss_size = 0 THEN nSection := 5 ELSE nSection := 6 END;
	Sys.Write2(out, nSection); (* NumberOfSections *)
	Sys.SeekRel(out, 4 * 3);
	Sys.Write2(out, 240);
	
	(* Characteristics *)
	IF B.CplFlag.main THEN Sys.Write2(out, 20H + 2 + 1)
	ELSE Sys.Write2(out, 2000H + 20H + 2)
	END;
	
	Sys.Write2(out, 20BH); (* Magic number for PE32+ *)
	Sys.SeekRel(out, 2);
	Sys.Write4(out, Linker.code_rawsize);
	k := Linker.data_rawsize + 200H * 2 + Linker.edata_rawsize;
	Sys.Write4(out, k);
	Sys.Write4(out, Linker.bss_size);
	Sys.Write4(out, Linker.code_rva + Linker.entry);
	Sys.Write4(out, Linker.code_rva);
	
	Sys.Write8(out, Linker.imagebase);
	Sys.Write4(out, 4096);
	Sys.Write4(out, 512);
	Sys.Write2(out, 5); (* MajorOSVer *)
	Sys.SeekRel(out, 2 * 3);
	Sys.Write2(out, 5);
	Sys.SeekRel(out, 2 + 4);
	k := 4096 + (4096 - Linker.code_size) MOD 4096 + Linker.code_size;
	k := k + Linker.bss_size + Linker.data_size + 4096 + 4096;
	k := k + (4096 - Linker.edata_size) MOD 4096 + Linker.edata_size;
	Sys.Write4(out, k);
	Sys.Write4(out, 400H);
	Sys.SeekRel(out, 4);
	IF B.CplFlag.console THEN Sys.Write2(out, 3) (* Subsys = Console *)
	ELSE Sys.Write2(out, 2) (* Subsys = GUI *)
	END;
	
	(* DLL Characteristics *)
	IF B.CplFlag.main THEN Sys.Write2(out, 0)
	ELSE Sys.Write2(out, 100H + 40H)
	END;
	
	Sys.Write8(out, 1000H); (* Size of stack reserve *)
	Sys.Write8(out, 1000H); (* Size of stack commit *)
	Sys.Write8(out, 10000H); (* Size of heap reserve *)
	Sys.SeekRel(out, 8 + 4);
	Sys.Write4(out, 16);
	
	Sys.Write4(out, Linker.edata_rva);
	Sys.Write4(out, Linker.edata_size);
	Sys.Write4(out, Linker.idata_rva);
	Sys.Write4(out, 130H);
	Sys.SeekRel(out, 8 * 3);
	Sys.Write4(out, Linker.reloc_rva);
	Sys.Write4(out, 12);
	Sys.SeekRel(out, 8 * 10);
	
	IF Linker.bss_size > 0 THEN
		Write_SectionHeader(
			'.bss', -1073741696, Linker.bss_rva, 0, Linker.bss_size, 0
		)
	END;
	Write_SectionHeader (
		'.data', -1073741760, Linker.data_rva, Linker.data_rawsize,
		Linker.data_size, Linker.data_fadr
	);
	Write_SectionHeader (
		'.text', 60000020H, Linker.code_rva, Linker.code_rawsize,
		Linker.code_size, Linker.code_fadr
	);
	Write_SectionHeader (
		'.idata', -1073741760, Linker.idata_rva, 200H,
		130H, Linker.idata_fadr
	);
	Write_SectionHeader (
		'.reloc', 42000040H, Linker.reloc_rva, 200H,
		12, Linker.reloc_fadr
	);
	Write_SectionHeader (
		'.edata', 40000040H, Linker.edata_rva, Linker.edata_rawsize,
		Linker.edata_size, Linker.edata_fadr
	);
END Write_PEHeader;

PROCEDURE Write_code_section;
	VAR b: BYTE; rider: Sys.MemFileRider; blk: Block;
BEGIN blk := procList.blk; Sys.SetMemFile(rider, blk.code, 0);
	REPEAT Sys.ReadMemFile(rider, b);
		IF ~rider.eof THEN Sys.Write1(out, b) END
	UNTIL rider.eof; ASSERT(blk.next = NIL)
END Write_code_section;

PROCEDURE Generate*(modinit: B.Node);
	VAR modkey: B.ModuleKey; n: INTEGER; str: B.String;
BEGIN
	(* Pass 1 *)
	AllocStaticData; ScanDeclaration(B.universe.first, 0);
	
	(* Pass 2 *)
	curProc := procList;
	WHILE curProc # NIL DO Procedure; curProc := curProc.next END;
	ModuleInit(modinit); MergeAllProcedure; DLLInit;
	
	(* Linker *)
	Sys.Seek(out, 400H - 16); modkey := B.modkey;
	Sys.Write8(out, modkey[0]); Sys.Write8(out, modkey[1]);

	IF B.CplFlag.main THEN Linker.imagebase := 400000H
	ELSE Linker.imagebase := 10000000H
	END;

	Linker.code_rawsize := (512 - CodeLen()) MOD 512 + CodeLen();
	Linker.code_size := CodeLen();
	
	Linker.data_size := staticSize; Align(Linker.data_size, 4096);
	Linker.data_rawsize := Linker.data_size;
	
	Linker.bss_size := varSize; Align(Linker.bss_size, 4096);
	
	Linker.bss_rva := 1000H;
	Linker.data_rva := Linker.bss_rva + Linker.bss_size;
	Linker.code_rva := Linker.data_rva + Linker.data_size;
	n := Linker.code_size; Align(n, 4096);
	Linker.idata_rva := Linker.code_rva + n;
	Linker.reloc_rva := Linker.idata_rva + 4096;
	Linker.edata_rva := Linker.reloc_rva + 4096;
	
	Linker.code_fadr := 400H;
	Linker.idata_fadr := Linker.code_fadr + Linker.code_rawsize;
	Linker.data_fadr := Linker.idata_fadr + 200H;
	Linker.reloc_fadr := Linker.data_fadr + Linker.data_rawsize;
	Linker.edata_fadr := Linker.reloc_fadr + 200H;
	
	Write_idata_section; Write_data_section;
	Write_reloc_section; Write_edata_section; Write_PEHeader;
	Sys.Seek(out, 400H); Write_code_section;
	Sys.Close(out);
	
	(* Rename files *)
	str[0] := 0X; B.AppendStr(modid, str);
	IF B.CplFlag.main THEN B.AppendStr('.exe', str)
	ELSE B.AppendStr('.dll', str)
	END;
	Sys.Delete(str); Sys.Rename(tempOutputName, str);

	(* Show statistics *)
	Sys.Console_WriteStr('No errors found.'); Sys.Console_WriteLn;
	Sys.Console_WriteStr('Code size: ');
	Sys.Console_WriteInt(Linker.code_size); Sys.Console_WriteLn;
	Sys.Console_WriteStr('Global variables size: ');
	Sys.Console_WriteInt(varSize); Sys.Console_WriteLn;
	Sys.Console_WriteStr('Static data size: ');
	Sys.Console_WriteInt(staticSize); Sys.Console_WriteLn;
	Linker.endTime := Sys.GetTickCount();
	Sys.Console_WriteStr('Compile time: ');
	Sys.Console_WriteInt(Linker.endTime - Linker.startTime);
	Sys.Console_WriteStr(' miliseconds'); Sys.Console_WriteLn;
	Sys.Console_WriteStr('Created binary file: ');
	Sys.Console_WriteStr(str); Sys.Console_WriteLn
END Generate;

BEGIN
	MakeItem0 := MakeItem
END Generator1.
