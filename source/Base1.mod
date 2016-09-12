MODULE Base1;

IMPORT
	SYSTEM, Sys := BaseSys, Crypt, S := Scanner1;

CONST
	MaxExt* = 8; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024;
	
	(* Object class *)
	cNull* = -1; cModule* = 0; cType* = 1;
	cNode* = 2; cVar* = 3; cRef* = 4; cConst* = 5;
	cProc* = 6; cField* = 8; cSProc* = 9; cSFunc* = 10;
	
	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;
	tNull* = 31;
	
	typEql* = {tBool, tSet, tPtr, tProc, tNil};
	typCmp* = {tInt, tReal, tChar, tStr};

TYPE
	IdStr* = S.IdStr;
	String* = S.String;
	ModuleKey* = ARRAY 2 OF INTEGER;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
    Node* = POINTER TO NodeDesc;
    Ident* = POINTER TO IdentDesc;
	
	TypeList = POINTER TO RECORD type: Type; next: TypeList END;
	
	ObjDesc* = EXTENSIBLE RECORD
		class*: INTEGER; type*: Type; ident*: Ident
	END;
	Const* = POINTER TO EXTENSIBLE RECORD (ObjDesc) val*: INTEGER END;
	Field* = POINTER TO EXTENSIBLE RECORD (ObjDesc) off*: INTEGER END;
	Var* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, lev*: INTEGER; ronly*, par*: BOOLEAN
	END;
	Proc* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, lev*, locblksize*: INTEGER;
        decl*: Ident; statseq*: Node; return*: Object
	END;
	Str* = POINTER TO EXTENSIBLE RECORD (Var)
		chars*: String; len*: INTEGER
	END;
	Module* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		export*: BOOLEAN; path*: String;
		name*: IdStr; key*: ModuleKey;
		lev*: INTEGER; first*: Ident;
		typeList*: TypeList;
	END;
	SProc* = POINTER TO EXTENSIBLE RECORD (ObjDesc) id*: IdStr END;
	
	IdentDesc* = RECORD
		export*: BOOLEAN;
		name*: IdStr; obj*: Object;
		next*: Ident
	END;
	
	Scope* = POINTER TO RECORD first*: Ident; dsc*: Scope END;
	
	NodeDesc* = EXTENSIBLE RECORD (ObjDesc)
		op*: INTEGER; left*, right*: Object; ronly*: BOOLEAN
	END;
	
	TypeDesc* = RECORD
		form*, size*, align*, nptr*: INTEGER;
		len*, lev*, adr*: INTEGER; base*: Type; fields*: Ident;
		parblksize*, nfpar*: INTEGER; obj*: Object;
		mod*, ref*: INTEGER (* import/export *)
	END;

VAR
	(* Predefined Types *)
	intType*, byteType*, realType*, longrealType*: Type;
	boolType*, setType*, charType*, nilType*, strType*: Type;
	noType*: Type; predefinedTypes: ARRAY 32 OF Type;
	
	topScope*, universe*: Scope;
	curLev*, modlev*: INTEGER; modkey*: ModuleKey;
	
	symfile: Sys.File;
	refno, preTypeNo, expno, modno: INTEGER;
	expList*: Ident; impTypes: ARRAY MaxExpTypes OF Type;
	modList*: ARRAY MaxImpMod OF Module;
	
	ExportType0: PROCEDURE(typ: Type);
	ImportType0: PROCEDURE(VAR typ: Type);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Read/Write for symfile *)

PROCEDURE WriteInt*(VAR f: Sys.File; n: INTEGER);
	VAR finish: BOOLEAN; b: INTEGER;
BEGIN
	REPEAT b := n MOD 128; finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128 END;
		Sys.Write1(f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt*(VAR f: Sys.File; VAR n: INTEGER);
	CONST MaxInt = 9223372036854775807; MinInt = -MaxInt - 1;
	VAR finish: BOOLEAN; i, b, k: INTEGER;
BEGIN n := 0; i := 1; k := 1;
	REPEAT Sys.Read1(f, b);
		IF i < 10 THEN
			finish := b >= 128; b := b MOD 128; n := n + b * k;
			IF i # 9 THEN k := k * 128 END; INC (i);
			IF finish & (b >= 64) THEN
				IF i # 9 THEN n := n + (-1 * k) ELSE n := n + MinInt END
			END
		ELSIF i = 10 THEN
			finish := TRUE; IF b = 127 THEN n := n + MinInt END
		ELSE ASSERT(FALSE)
		END
	UNTIL finish
END ReadInt;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE NewVar*(tp: Type): Var;
	VAR v: Var;
BEGIN
	NEW(v); v.class := cVar; v.type := tp; v.lev := curLev;
	v.ronly := FALSE; v.par := FALSE;
	RETURN v
END NewVar;

PROCEDURE NewConst*(tp: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); c.class := cConst; c.type := tp; c.val := val;
	RETURN c
END NewConst;

PROCEDURE NewPar*(proc, tp: Type; cls: INTEGER; ronly: BOOLEAN): Var;
	VAR v: Var;
BEGIN
	NEW(v); v.class := cls; v.type := tp; v.lev := curLev;
	v.ronly := ronly; v.par := TRUE;
	INC(proc.nfpar);
	RETURN v
END NewPar;

PROCEDURE NewField*(rec, tp: Type): Field;
	VAR fld: Field;
BEGIN
	NEW(fld); fld.class := cField; fld.type := tp;
	rec.nptr := rec.nptr + tp.nptr;
	RETURN fld
END NewField;

PROCEDURE NewStr*(str: String; slen: INTEGER): Str;
	VAR x: Str;
BEGIN
	NEW(x); x.class := cVar; x.type := strType; x.lev := 0;
	x.par := FALSE; x.ronly := TRUE;
	x.chars := str; x.len := slen;
	RETURN x
END NewStr;

PROCEDURE NewProc*(): Proc;
	VAR p: Proc;
BEGIN
	NEW(p); p.class := cProc; p.lev := curLev;
	RETURN p
END NewProc;

PROCEDURE NewTypeObj*(tp: Type): Object;
	VAR x: Object;
BEGIN
	NEW(x); x.class := cType; x.type := tp; tp.obj := x;
	RETURN x
END NewTypeObj;

PROCEDURE NewSProc*(name: IdStr; cls: INTEGER): SProc;
	VAR x: SProc;
BEGIN
	NEW(x); x.id := name; x.class := cls; x.type := noType;
	RETURN x
END NewSProc;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewType*(VAR typ: Type; form: INTEGER);
BEGIN
	NEW(typ); typ.form := form; typ.nptr := 0;
	typ.mod := -1; typ.ref := -1
END NewType;

PROCEDURE NewArray*(len: INTEGER): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tArray); tp.len := len;
	RETURN tp
END NewArray;

PROCEDURE CompleteArray*(tp: Type);
BEGIN
	IF tp.base.form = tArray THEN CompleteArray(tp) END;
	tp.nptr := tp.len * tp.base.nptr
END CompleteArray;

PROCEDURE NewRecord*(): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tRec); tp.len := 0; tp.lev := curLev;
	RETURN tp
END NewRecord;

PROCEDURE ExtendRecord*(recType: Type);
BEGIN
	recType.len := recType.base.len + 1;
	recType.nptr := recType.base.nptr
END ExtendRecord;

PROCEDURE NewPointer*(): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tPtr); tp.nptr := 1;
	RETURN tp
END NewPointer;

PROCEDURE NewProcType*(): Type;
	VAR tp: Type;
BEGIN
	NewType(tp, tProc); tp.nfpar := 0;
	RETURN tp
END NewProcType;

PROCEDURE NewPredefinedType(VAR typ: Type; form: INTEGER);
BEGIN
	NewType(typ, form); INC(preTypeNo);
	typ.mod := -2; typ.ref := preTypeNo;
	predefinedTypes[preTypeNo] := typ
END NewPredefinedType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*;
	VAR scp: Scope;
BEGIN NEW(scp); scp.dsc := topScope; topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

PROCEDURE IncLev*(n: INTEGER);
BEGIN curLev := curLev + n
END IncLev;

PROCEDURE Enter (x: Object; name: IdStr);
	VAR ident: Ident;
BEGIN
	NEW(ident); ident.name := name; ident.export := FALSE;
	ident.obj := x; x.ident := ident;
	ident.next := topScope.first; topScope.first := ident
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Export symbol file *)

PROCEDURE NewExport(VAR ident: Ident);
	VAR p: Ident;
BEGIN NEW(ident); INC(expno);
	IF expList = NIL THEN expList := ident
	ELSE p := expList;
		WHILE p.next # NIL DO p := p.next END;
		p.next := ident
	END
END NewExport;

PROCEDURE DetectType(typ: Type);
BEGIN
	IF typ # NIL THEN
		WriteInt(symfile, typ.mod); WriteInt(symfile, typ.ref);
		IF typ.mod >= 0 THEN Sys.WriteStr(symfile, modList[typ.mod].name) END;
		IF (typ.mod = -1) & (typ.ref < 0) THEN ExportType0(typ) END
	ELSE WriteInt(symfile, -2); WriteInt(symfile, 0)
	END
END DetectType;

PROCEDURE ExportProc(typ: Type);
	VAR par: Ident; x: Var;
BEGIN
	DetectType(typ.base); WriteInt(symfile, typ.nfpar); par := typ.fields;
	WHILE par # NIL DO x := par.obj(Var);
		WriteInt(symfile, x.class);
		Sys.WriteStr(symfile, par.name);
		WriteInt(symfile, ORD(x.ronly));
		DetectType(x.type);
		par := par.next
	END;
	WriteInt(symfile, cType)
END ExportProc;
	
PROCEDURE ExportType(typ: Type);
	VAR fld, ident: Ident; s: String;
BEGIN
	IF refno < MaxExpTypes THEN typ.ref := refno; INC(refno)
	ELSE S.Mark('Too many exported types')
	END;
	WriteInt(symfile, typ.ref);
	IF typ.form = tRec THEN 
		NewExport(ident); NEW(ident.obj); ident.obj.class := cType;
		ident.obj.type := typ; WriteInt(symfile, expno)
	ELSE WriteInt(symfile, 0)
	END;
	WriteInt(symfile, typ.form);
	IF typ.form = tRec THEN
		DetectType(typ.base);
		fld := typ.fields;
		WHILE fld # NIL DO
			IF fld.export OR (fld.obj.type.nptr > 0) THEN
				WriteInt(symfile, cField);
				IF ~fld.export THEN s[0] := 0X; Sys.WriteStr(symfile, s)
				ELSE Sys.WriteStr(symfile, fld.name)
				END;
				DetectType(fld.obj.type)
			END;
			fld := fld.next
		END;
		WriteInt (symfile, cType)
	ELSIF typ.form = tArray THEN
		WriteInt(symfile, typ.len);
		DetectType (typ.base)
	ELSIF typ.form = tPtr THEN
		DetectType(typ.base)
	ELSIF typ.form = tProc THEN
		ExportProc(typ)
	END
END ExportType;

PROCEDURE WriteModkey(key: ModuleKey);
BEGIN
	Sys.Write8(symfile, key[0]);
	Sys.Write8(symfile, key[1])
END WriteModkey;

PROCEDURE WriteSymfile*;
	VAR ident, exp: Ident; i, k, n, size: INTEGER; mod: Module;
		hash: Crypt.MD5Hash; chunk: ARRAY 64 OF BYTE;
BEGIN
	refno := 0; expno := 0;
	Sys.Rewrite(symfile, 'sym.temp_'); Sys.Seek(symfile, 16);
	WriteInt (symfile, modlev);
	
	FOR i := 0 TO modno-1 DO
		mod := modList[0];
		IF mod.export THEN
			WriteInt(symfile, cModule);
			Sys.WriteStr(symfile, mod.name);
			WriteModkey(mod.key)
		END
	END;
	
	ident := universe.first;
	WHILE ident # NIL DO
		IF ident.export THEN
			IF ident.obj.class = cConst THEN
				WriteInt(symfile, cConst);
				Sys.WriteStr(symfile, ident.name);
				WriteInt(symfile, ident.obj(Const).val);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cType THEN
				WriteInt(symfile, cType);
				Sys.WriteStr(symfile, ident.name);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cVar THEN
				WriteInt(symfile, cVar);
				Sys.WriteStr(symfile, ident.name);
				NewExport(exp); exp.obj := ident.obj;
				WriteInt (symfile, expno);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cProc THEN
				WriteInt(symfile, cProc);
				Sys.WriteStr(symfile, ident.name);
				NewExport(exp); exp.obj := ident.obj;
				WriteInt(symfile, expno);
				ExportProc(ident.obj.type)
			ELSE ASSERT(FALSE)
			END
		END;
		ident := ident.next
	END;
	WriteInt(symfile, cNull);
	
	size := Sys.FilePos(symfile); Sys.Seek(symfile, 0);
	Crypt.InitMD5Hash(hash); i := 0;
	REPEAT k := 0;
		REPEAT Read_byte(symfile, n); chunk[k] := n; INC(i); INC(k)
		UNTIL (i = size) OR (k = 64);
		Crypt.MD5ComputeChunk(hash, chunk, k * 8)
	UNTIL i = size;
	
	Sys.Seek(symfile, 0);
	modkey[0] := Crypt.MD5GetLowResult(hash);
	modkey[1] := Crypt.MD5GetHighResult(hash);
	WriteModkey(modkey);
	Sys.Close(symfile)
END WriteSymfile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)

PROCEDURE FindModule(modname: IdStr): Module;
	VAR i: INTEGER;
BEGIN
	
END FindModule;

PROCEDURE DetectTypeI(VAR typ: Type);
	VAR mod, ref: INTEGER; modname: IdStr;
		module: Module; p: TypeList;
BEGIN
	ReadInt(symfile, mod); ReadInt(symfile, ref);
	IF mod = -2 THEN typ := predefinedTypes[ref]
	ELSIF mod = -1 THEN
		IF ref >= 0 THEN typ := impTypes[ref] ELSE ImportType0(typ) END
	ELSIF typ.mod >= 0 THEN
		Sys.ReadStr(sym, modname); module := FindModule(modname);
		p := module.types; WHILE ref > 0 DO DEC(ref); p := p.next END;
		typ := p.type
	END
END DetectTypeI;

PROCEDURE ImportProc(VAR typ: Type);
	VAR par: Ident; x: Var; xtype: Type;
		cls, n: INTEGER; name: IdStr; ronly: BOOLEAN;
BEGIN
	typ := NewProcType(); DetectTypeI(typ.base);
	ReadInt(symfile, cls); OpenScope;
	WHILE cls # cType DO
		Sys.ReadStr(symfile, name);
		ReadInt(symfile, n); ronly := n = ORD(TRUE);
		DetectTypeI(xtype);
		x := NewPar(typ, xtype, cls, ronly);
		IF par # NIL THEN NEW(par.next); par := par.next
		ELSE NEW(topScope.first); par := topScope.first
		END;
		par.export := FALSE; par.obj := x; par.name := name
	END;
	typ.fields := topScope.first; CloseScope
END ImportProc;
	
PROCEDURE ImportType(VAR typ: Type);
	VAR fld: Ident; x: Object; name: IdStr;
		fltype: Type; cls, form, ref, exp, len: INTEGER;
BEGIN
	ReadInt(symfile, ref); ReadInt(symfile, exp);
	ReadInt(symfile, form);
	IF form = tRec THEN
		typ := NewRecord(); typ.ref := ref; typ.mod := -(curLev+1);
		typ.adr := exp; DetectTypeI(typ.base); ExtendRecord(typ);
		ReadInt(symfile, cls); OpenScope;
		WHILE cls # cType DO
			Sys.ReadStr(symfile, name);
			DetectTypeI(fltype);
			x := NewField(typ, fltype);
			IF fld # NIL THEN NEW(fld.next); fld := fld.next
			ELSE NEW(topScope.first); fld := topScope.first
			END;
			fld.export := FALSE; fld.obj := x; fld.name := name
		END;
		typ.fields := topScope.first; CloseScope
	ELSIF form = tArray THEN
		ReadInt(symfile, len); typ := NewArray(len);
		DetectTypeI(typ.base); CompleteArray(typ)
	ELSIF typ.form = tPtr THEN
		typ := NewPointer(); DetectTypeI(typ.base)
	ELSIF typ.form = tProc THEN
		ImportProc(typ)
	END
END ImportType;

PROCEDURE ReadModkey(key: ModuleKey);
BEGIN
	Sys.Read8(symfile, key[0]);
	Sys.Read8(symfile, key[1])
END WriteModkey;

PROCEDURE ImportModules*;
	VAR ident, exp: Ident; cls, i, k, n, size: INTEGER; module: Module;
		fname: String; errflag: BOOLEAN;
BEGIN
	fname := modname; i := 0; WHILE fname[i] # 0X DO INC(i) END;
	fname[i] := '.'; fname[i+1] := 's'; fname[i+2] := 'y';
	fname[i+3] := 'm'; fname[i+4] := 0X;
	IF Sys.Existed(fname) THEN
		NEW(module); module.export := FALSE; module.name := modname;
		modident.obj := module; modList[modno] := module; INC(modno);
	
		Sys.Open(symfile, fname);
		ReadModkey(module.key);
		ReadInt(symfile, module.lev);
		
		ReadInt(symfile, cls); errflag := FALSE;
		WHILE cls = cModule DO
			
		END;
		
		FOR i := 0 TO nmod-1 DO
			mod := modList[0];
			IF mod.export THEN
				WriteInt(symfile, cModule);
				Sys.WriteStr(symfile, mod.name);
				WriteModkey(mod.key)
			END
		END;
		
		ident := universe.first;
		WHILE ident # NIL DO
			IF ident.export THEN
				IF ident.obj.class = cConst THEN
					WriteInt(symfile, cConst);
					Sys.WriteStr(symfile, ident.name);
					WriteInt(symfile, ident.obj(Const).val);
					DetectType(ident.obj.type)
				ELSIF ident.obj.class = cType THEN
					WriteInt(symfile, cType);
					Sys.WriteStr(symfile, ident.name);
					DetectType(ident.obj.type)
				ELSIF ident.obj.class = cVar THEN
					WriteInt(symfile, cVar);
					Sys.WriteStr(symfile, ident.name);
					NewExport(exp); exp.obj := ident.obj;
					WriteInt (symfile, expno);
					DetectType(ident.obj.type)
				ELSIF ident.obj.class = cProc THEN
					WriteInt(symfile, cProc);
					Sys.WriteStr(symfile, ident.name);
					NewExport(exp); exp.obj := ident.obj;
					WriteInt(symfile, expno);
					ExportProc(ident.obj.type)
				ELSE ASSERT(FALSE)
				END
			END;
			ident := ident.next
		END;
		WriteInt(symfile, cNull);
		
		size := FilePos(symfile); Seek(symfile, 0);
		Crypt.InitMD5Hash(hash); i := 0;
		REPEAT k := 0;
			REPEAT Read_byte(symfile, n); chunk[k] := n; INC(i); INC(k)
			UNTIL (i = size) OR (k = 64);
			Crypt.MD5ComputeChunk(hash, chunk, k * 8)
		UNTIL i = size;
		
		Seek(symfile, 0);
		modkey[0] := Crypt.MD5GetLowResult(hash);
		modkey[1] := Crypt.MD5GetHighResult(hash);
		WriteModkey(modkey);
		Close(symfile)
END WriteSymfile;

PROCEDURE NewModule*(modident: Ident; modname: IdStr);
	VAR fname
BEGIN
	fname := modname; i := 0; WHILE fname[i] # 0X DO INC(i) END;
	fname[i] := '.'; fname[i+1] := 's'; fname[i+2] := 'y';
	fname[i+3] := 'm'; fname[i+4] := 0X;
	IF Sys.Existed(fname) THEN
END NewModule;

PROCEDURE Init*;
BEGIN
	NEW(universe); topScope := universe; curLev := 0; modno := 0;
	
	Enter(NewTypeObj(intType), 'INTEGER');
	Enter(NewTypeObj(byteType), 'BYTE');
	Enter(NewTypeObj(realType), 'REAL');
	Enter(NewTypeObj(setType), 'SET');
	Enter(NewTypeObj(boolType), 'BOOLEAN');
	Enter(NewTypeObj(charType), 'CHAR');
	
	Enter(NewSProc('INC', cSProc), 'INC');
	Enter(NewSProc('DEC', cSProc), 'DEC');
	Enter(NewSProc('INCL', cSProc), 'INCL');
	Enter(NewSProc('EXCL', cSProc), 'EXCL');
	Enter(NewSProc('NEW', cSProc), 'NEW');
	Enter(NewSProc('ASSERT', cSProc), 'ASSERT');
	Enter(NewSProc('PACK', cSProc), 'PACK');
	Enter(NewSProc('UNPK', cSProc), 'UNPK');
	
	Enter(NewSProc('ABS', cSFunc), 'ABS');
	Enter(NewSProc('ODD', cSFunc), 'ODD');
	Enter(NewSProc('LEN', cSFunc), 'LEN');
	Enter(NewSProc('LSL', cSFunc), 'LSL');
	Enter(NewSProc('ASR', cSFunc), 'ASR');
	Enter(NewSProc('ROR', cSFunc), 'ROR');
	Enter(NewSProc('FLOOR', cSFunc), 'FLOOR');
	Enter(NewSProc('FLT', cSFunc), 'FLT');
	Enter(NewSProc('ORD', cSFunc), 'ORD');
	Enter(NewSProc('CHR', cSFunc), 'CHR')
END Init;

BEGIN
	ExportType0 := ExportType; ImportType0 := ImportType;

	preTypeNo := 0; predefinedTypes[0] := NIL; (* type no. 0 is no-type *)
	NewPredefinedType(intType, tInt);
	NewPredefinedType(byteType, tInt);
	NewPredefinedType(boolType, tBool);
	NewPredefinedType(setType, tSet);
	NewPredefinedType(charType, tChar);
	NewPredefinedType(nilType, tNil);
	NewPredefinedType(realType, tReal);
	NewPredefinedType(longrealType, tReal);
	NewPredefinedType(strType, tStr);
	NewPredefinedType(noType, tNull)
END Base1.