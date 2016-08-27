MODULE Generator0;

IMPORT
	Base := Base0,
	Scanner := Scanner0,
	SymTable := SymTable0;
	
TYPE
	CallItem* = RECORD
		rtype*: Base.Type;
		fpar*: Base.Object;
		nact*, nfpar*: INTEGER
	END;
	
VAR
	pc*: INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Machine code emitter *)

PROCEDURE Reset_code_buffer;
BEGIN Emit.i := 0; Emit.oldi := 0; pc := 1; metacode[pc].relfixup := FALSE
END Reset_code_buffer;

PROCEDURE Put_byte(n: INTEGER);
BEGIN code[Emit.i] := n MOD 256; INC(Emit.i)
END Put_byte;

PROCEDURE Put_2bytes(n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; Emit.i := Emit.i + 2
END Put_2bytes;

PROCEDURE Put_4bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; n := n DIV 256;
	code[Emit.i + 2] := n MOD 256; n := n DIV 256;
	code[Emit.i + 3] := n MOD 256; Emit.i := Emit.i + 4
END Put_4bytes;

PROCEDURE Put_8bytes (n: INTEGER);
BEGIN
	code[Emit.i] := n MOD 256; n := n DIV 256;
	code[Emit.i + 1] := n MOD 256; n := n DIV 256;
	code[Emit.i + 2] := n MOD 256; n := n DIV 256;
	code[Emit.i + 3] := n MOD 256; n := n DIV 256;
	code[Emit.i + 4] := n MOD 256; n := n DIV 256;
	code[Emit.i + 5] := n MOD 256; n := n DIV 256;
	code[Emit.i + 6] := n MOD 256; n := n DIV 256;
	code[Emit.i + 7] := n MOD 256; Emit.i := Emit.i + 8
END Put_8bytes;
	
PROCEDURE Emit_REX_prefix (reg, rsize: INTEGER);
	CONST W_bit = 8; R_bit = 4; X_bit = 2; B_bit = 1;
	VAR rex: INTEGER;
BEGIN
	rex := 40H;
	IF rsize = 8 THEN rex := rex + W_bit END;
	IF reg >= reg_R8 THEN rex := rex + R_bit END;
	IF (Emit.mem.rm >= reg_R8)
	OR (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP) & (Emit.mem.bas >= reg_R8)
	THEN rex := rex + B_bit
	END;
	IF (Emit.mem.mod # 3) & (Emit.mem.rm = reg_SP) & (Emit.mem.idx >= reg_R8)
	THEN rex := rex + X_bit
	END;
	IF (rex # 40H)
	OR (rsize = 1) &
		((reg IN {reg_SP .. reg_DI})
		OR (Emit.mem.mod = 3) & (Emit.mem.rm IN {reg_SP .. reg_DI}))
	THEN Put_byte (rex)
	END
END Emit_REX_prefix;

PROCEDURE Emit_16bit_prefix (rsize : INTEGER);
BEGIN IF rsize = 2 THEN Put_byte (66H) END
END Emit_16bit_prefix;

PROCEDURE Handle_multibytes_opcode (VAR op: INTEGER);
BEGIN
	IF op MOD 256 = 0FH THEN
		Put_byte (0FH); op := op DIV 256;
		IF (op MOD 256 = 38H) OR (op MOD 256 = 3AH) THEN
			Put_byte (op); op := op DIV 256
		END
	END
END Handle_multibytes_opcode;

PROCEDURE Emit_ModRM (reg: INTEGER);
BEGIN
	Put_byte (Emit.mem.mod * 64 + reg MOD 8 * 8 + Emit.mem.rm MOD 8);
	IF Emit.mem.mod # 3 THEN
		IF Emit.mem.rm IN {reg_SP, reg_R12} THEN
			Put_byte (
				Emit.mem.scl * 64 + Emit.mem.idx MOD 8 * 8 + Emit.mem.bas MOD 8
			)
		END;
		metacode[pc].dispPos := Emit.i - Emit.oldi;
		IF (Emit.mem.mod = 0) & (Emit.mem.rm IN {reg_BP, reg_R13})
		OR (Emit.mem.mod = 2)
		THEN Put_4bytes (Emit.mem.disp)
		ELSIF Emit.mem.mod = 1 THEN Put_byte (Emit.mem.disp)
		END
	END
END Emit_ModRM;

PROCEDURE Next_inst;
BEGIN
	metacode[pc].ip := ip; metacode[pc].size := Emit.i - Emit.oldi;
	ip := ip + Emit.i - Emit.oldi; INC (pc); Emit.oldi := Emit.i;
	metacode[pc].relfixup := FALSE
END Next_inst;

(* -------------------------------------------------------------------------- *)

PROCEDURE IntToSet (n: INTEGER) : SET;
	RETURN SYSTEM.VAL(SET, n)
END IntToSet;
	
PROCEDURE EmitRegRm (op, reg, rsize: INTEGER);
	CONST w_bit = 1;
	VAR org: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (reg, rsize);
	org := op; Handle_multibytes_opcode (op);
	
	IF (rsize > 1) & (org < LEA) THEN op := op + w_bit END;
	Put_byte (op); Emit_ModRM (reg);
	
	Next_inst
END EmitRegRm;

PROCEDURE EmitRm (op, rsize: INTEGER);
	CONST w_bit = 1;
	VAR op3bits, org: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	org := op; Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF (rsize > 1) & (IntToSet(op) * IntToSet(w_bit) = {}) & (org # LDMXCSR)
		& (org # STMXCSR)
	THEN op := op + w_bit
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	Next_inst
END EmitRm;

PROCEDURE EmitRmImm (op, rsize, imm: INTEGER);
	CONST w_bit = 1; s_bit = 2;
	VAR op3bits: INTEGER;
BEGIN
	Emit_16bit_prefix (rsize);
	IF op MOD 256 # IMULi THEN Emit_REX_prefix (0, rsize)
	ELSE Emit_REX_prefix (op DIV 256, rsize)
	END;
	Handle_multibytes_opcode (op);
	
	op3bits := op DIV 256; op := op MOD 256;
	IF rsize > 1 THEN
		IF (op = 0C0H) OR (op = 0BAH) THEN rsize := 1
		ELSIF (imm >= -128) & (imm <= 127) & (op = 80H) THEN
			op := op + s_bit; rsize := 1
		END;
		IF (IntToSet(op) * IntToSet(w_bit) = {}) & (op # 0BAH) THEN
			op := op + w_bit
		END
	END;
	Put_byte (op); Emit_ModRM (op3bits);
	
	IF rsize = 1 THEN Put_byte (imm) ELSIF rsize = 2 THEN Put_2bytes (imm)
	ELSE Put_4bytes (imm)
	END;
	
	Next_inst
END EmitRmImm;

PROCEDURE EmitBare (op: INTEGER);
BEGIN WHILE op > 0 DO Put_byte (op); op := op DIV 256 END; Next_inst
END EmitBare;

PROCEDURE EmitXmmRm (op, xreg, rsize: INTEGER);
	VAR prefix : INTEGER;
BEGIN
	prefix := op MOD 256; op := op DIV 256;
	IF prefix # 0 THEN Put_byte (prefix) END;
	Emit_REX_prefix (xreg, rsize); Handle_multibytes_opcode (op);
	Put_byte (op MOD 256); Emit_ModRM (xreg); Next_inst
END EmitXmmRm;

PROCEDURE EmitMOVZX (reg, rmsize: INTEGER);
	VAR rsize, op: INTEGER;
BEGIN rsize := 4; op := 0B6H;
	IF rmsize = 1 THEN
		IF (Emit.mem.mod = 3) & (Emit.mem.rm IN {reg_SP .. reg_DI})
		THEN rsize := 8
		END
	ELSIF rmsize = 2 THEN INC (op)
	ELSE ASSERT(FALSE)
	END;
	Emit_REX_prefix (reg, rsize);
	Put_byte (0FH); Put_byte (op);
	Emit_ModRM (reg);
	Next_inst
END EmitMOVZX;

PROCEDURE EmitMOVSX (reg, rmsize: INTEGER);
	VAR op: INTEGER;
BEGIN 
	IF rmsize = 1 THEN op := 0BE0FH
	ELSIF rmsize = 2 THEN op := 0BF0FH
	ELSIF rmsize = 4 THEN op := 63H
	ELSE ASSERT(FALSE)
	END;
	Emit_REX_prefix (reg, 8);
	Handle_multibytes_opcode (op); Put_byte (op);
	Emit_ModRM (reg);
	Next_inst
END EmitMOVSX;

(* -------------------------------------------------------------------------- *)

PROCEDURE SetRmOperand_reg (reg: INTEGER);
BEGIN Emit.mem.rm := reg; Emit.mem.mod := 3
END SetRmOperand_reg;

PROCEDURE SetRmOperand_regI (reg, disp: INTEGER);
BEGIN
	Emit.mem.rm := reg; Emit.mem.disp := disp;
	IF (disp >= -128) & (disp <= 127) THEN
		IF (disp = 0) & ~ (reg IN {reg_BP, reg_R13}) THEN Emit.mem.mod := 0
		ELSE Emit.mem.mod := 1
		END
	ELSE Emit.mem.mod := 2
	END;
	IF reg IN {reg_SP, reg_R12} THEN
		Emit.mem.bas := reg_SP; Emit.mem.idx := reg_SP; Emit.mem.scl := 0
	END
END SetRmOperand_regI;

PROCEDURE SetRmOperand_staticvar (adr: INTEGER);
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
END SetRmOperand;

(* -------------------------------------------------------------------------- *)

PROCEDURE EmitRR (op, reg, rsize, rm: INTEGER);
BEGIN SetRmOperand_reg (rm); EmitRegRm (op, reg, rsize)
END EmitRR;

PROCEDURE EmitRI (op, rm, rsize, imm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); IF op = IMULi THEN op := op + rm * 256 END;
	EmitRmImm (op, rsize, imm)
END EmitRI;

PROCEDURE EmitR (op, rm, rsize: INTEGER);
BEGIN SetRmOperand_reg (rm); EmitRm (op, rsize)
END EmitR;

(* -------------------------------------------------------------------------- *)

PROCEDURE MoveRI (rm, rsize, imm : INTEGER);
	CONST w_bit = 8;
	VAR op: INTEGER;
BEGIN
	SetRmOperand_reg (rm); Emit_16bit_prefix (rsize);
	Emit_REX_prefix (0, rsize); op := 0B0H + rm MOD 8;
	IF rsize > 1 THEN op := op + w_bit END; Put_byte (op); 
	IF rsize = 1 THEN Put_byte (imm) ELSIF rsize = 2 THEN Put_2bytes (imm)
	ELSIF rsize = 4 THEN Put_4bytes (imm) ELSE Put_8bytes (imm)
	END;
	Next_inst
END MoveRI;

PROCEDURE PushR (rm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (50H + rm MOD 8);
	ProcState.memstack := ProcState.memstack + 8; Next_inst
END PushR;

PROCEDURE PopR (rm: INTEGER);
BEGIN
	SetRmOperand_reg (rm); Emit_REX_prefix (0, 4); Put_byte (58H + rm MOD 8);
	ProcState.memstack := ProcState.memstack - 8; Next_inst
END PopR;

PROCEDURE Branch (disp: INTEGER);
BEGIN
	Put_byte (0E9H); metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); Next_inst
END Branch;

PROCEDURE CallNear (disp: INTEGER);
BEGIN
	Put_byte (0E8H); metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); metacode[pc].relfixup := TRUE; Next_inst
END CallNear;

PROCEDURE CondBranch (cond, disp: INTEGER);
BEGIN
	Put_byte (0FH); Put_byte (80H + cond);
	metacode[pc].dispPos := Emit.i - Emit.oldi;
	Put_4bytes (disp); Next_inst
END CondBranch;

PROCEDURE SetccRm (cond: INTEGER);
BEGIN
	Emit_REX_prefix (0, 1); Put_byte (0FH); Put_byte (90H + cond);
	Emit_ModRM (0); Next_inst
END SetccRm;

PROCEDURE EmitRep (op, rsize, z: INTEGER);
	CONST w_bit = 1;
BEGIN
	Put_byte (0F2H + z); (* REP prefix *)
	Emit_16bit_prefix (rsize); Emit_REX_prefix (0, rsize);
	IF (rsize > 1) & (IntToSet(op) * IntToSet(w_bit) = {}) THEN
		op := op + w_bit
	END;
	Put_byte (op); Next_inst
END EmitRep;

PROCEDURE Write_to_file* (from, to: INTEGER);
	VAR	org, p, i, k: INTEGER;
		
	PROCEDURE Fixup_disp (p, i: INTEGER);
		VAR disp: CARD32;
	BEGIN
		disp := 0; i := i + metacode[p].dispPos;
		SYSTEM.GET (SYSTEM.ADR(code[i]), disp);
		disp := disp - metacode[p].ip - metacode[p].size - ProcState.prologsize;
		SYSTEM.PUT (SYSTEM.ADR(code[i]), disp)
	END Fixup_disp;
	
BEGIN (* Write_to_file *)
	org := metacode[from].ip - ProcState.adr; p := from; i := org; k := 0;
	WHILE p <= to DO
		IF metacode[p].relfixup THEN Fixup_disp (p, i) END;
		k := k + metacode[p].size; i := org + k; INC (p)
	END;
	Base.Write_bytes2 (out, SYSTEM.ADR(code[org]), k)
END Write_to_file;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FitInReg*(tp: Base.Type): BOOLEAN;
	RETURN (tp.size = 1) & (tp.size = 2) & (tp.size = 4) & (tp.size = 8)
END FitInReg;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE MakeItem*(VAR x: Base.Item; obj: Base.Object);
END MakeItem;

PROCEDURE MakeConst*(VAR x: Base.Item; tp: Base.Type; val: INTEGER);
END MakeConst;

PROCEDURE MakeStr*(
	VAR x: Base.Item; str: Base.String; slen: INTEGER; ansi: BOOLEAN
);
END MakeStr;

PROCEDURE StrToChar*(VAR x: Base.Item);
END StrToChar;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE FixLink*(L: INTEGER);
END FixLink;

PROCEDURE Fixup*(x: Base.Item);
END Fixup;

PROCEDURE FJump*(VAR L: INTEGER);
END FJump;

PROCEDURE CFJump*(VAR x: Base.Item);
END CFJump;
	
PROCEDURE CBJump*(VAR x: Base.Item; L: INTEGER);
END CBJump;
  
PROCEDURE BJump*(L: INTEGER);
END BJump;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Store*(VAR x, y: Base.Item);
END Store;

PROCEDURE StoreStruct*(VAR x, y: Base.Item);
END StoreStruct;

PROCEDURE CopyStr*(VAR x, y: Base.Item);
END CopyStr;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE LoadVolatile*(VAR x: Base.Item);
END LoadVolatile;

PROCEDURE Negate*(VAR x: Base.Item);
END Negate;

PROCEDURE Not*(VAR x: Base.Item);
END Not;

PROCEDURE IntAdd*(VAR x, y: Base.Item; op: INTEGER);
END IntAdd;

PROCEDURE IntMul*(VAR x, y: Base.Item);
END IntMul;

PROCEDURE IntDiv*(VAR x, y: Base.Item; div: BOOLEAN);
END IntDiv;

PROCEDURE RealOp*(VAR x, y: Base.Item; op: INTEGER);
END RealOp;

PROCEDURE SetOp*(VAR x, y: Base.Item; op: INTEGER);
END SetOp;

PROCEDURE Or1*(VAR x: Base.Item);
END Or1;

PROCEDURE Or2*(VAR x, y: Base.Item);
END Or2;

PROCEDURE And1*(VAR x: Base.Item);
END And1;

PROCEDURE And2*(VAR x, y: Base.Item);
END And2;

PROCEDURE Compare*(VAR x, y: Base.Item; rel: INTEGER);
END Compare;

PROCEDURE RealCompare*(VAR x, y: Base.Item; rel: INTEGER);
END RealCompare;

PROCEDURE StrCompare*(VAR x, y: Base.Item; rel: INTEGER);
END StrCompare;

PROCEDURE Membership*(VAR x, y: Base.Item);
END Membership;

PROCEDURE TypeTest*(VAR x: Base.Item; tp: Base.Type; guard: BOOLEAN);
END TypeTest;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Set1*(VAR x: Base.Item);
END Set1;

PROCEDURE Set2*(VAR x, y: Base.Item);
END Set2;

PROCEDURE Set3*(VAR x, y, z: Base.Item);
END Set3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Deref*(VAR x: Base.Item);
END Deref;

PROCEDURE Field*(VAR x: Base.Item; field: Base.Object);
END Field;

PROCEDURE Index*(VAR x, idx: Base.Item);
END Index;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE For1*(VAR z: Base.Item);
END For1;

PROCEDURE For2*(x, z: Base.Item; inc: INTEGER; VAR L2: INTEGER);
END For2;

PROCEDURE For3*(VAR x: Base.Item; inc, L, L2: INTEGER);
END For3;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE PrepareCall*(VAR x: Base.Item; VAR c: CallItem);
END PrepareCall;

PROCEDURE Call*(VAR c: CallItem);
END Call;

PROCEDURE ReturnValue*(VAR x: Base.Item);
END ReturnValue;

PROCEDURE ValPar*(VAR x: Base.Item; VAR c: CallItem);
END ValPar;

PROCEDURE RefPar*(VAR x: Base.Item; VAR c: CallItem);
END RefPar;

PROCEDURE RecPar*(VAR x: Base.Item; VAR c: CallItem);
END RecPar;

PROCEDURE ArrayPar*(VAR x: Base.Item; VAR c: CallItem);
END ArrayPar;

PROCEDURE ByteArrayPar*(VAR x: Base.Item; VAR c: CallItem);
END ByteArrayPar;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE Enter*;
END Enter;

PROCEDURE Return*;
END Return;
	
PROCEDURE Init*;
END Init;

PROCEDURE Finish*;
END Finish;

END Generator0.