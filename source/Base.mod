MODULE Base;

IMPORT
	SYSTEM, Sys := BaseSys, Crypt, S := Scanner;

CONST
	MaxExt* = 7; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024; MaxModLev* = 255;
	
	(* Object class *)
	cNull* = -1; cModule* = 0; cType* = 1;
	cNode* = 2; cVar* = 3; cConst* = 4; cProc* = 5;
	cField* = 6; cSProc* = 7; cSFunc* = 8;
	
	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;
	tNull* = 31;
	
	typScalar* = {tInt, tBool, tSet, tChar, tReal, tPtr, tProc, tNil};
	typEql* = {tBool, tSet, tPtr, tProc, tNil};
	typCmp* = {tInt, tReal, tChar, tStr};
	
	(* SProc id *)
	spINC* = S.spINC; spDEC* = S.spDEC; spINCL* = S.spINCL; spEXCL* = S.spEXCL;
	spNEW* = 4; spASSERT* = 5; spPACK* = 6; spUNPK* = 7;
	sfABS* = 8; sfODD* = 9; sfLEN* = 10;
	sfLSL* = S.sfLSL; sfASR* = S.sfASR; sfROR* = S.sfROR;
	sfFLOOR* = 14; sfFLT* = 15; sfORD* = 16; sfCHR* = 17;
	
	spGET* = 18; spPUT* = 19; spCOPY* = 20;
	spLoadLibraryW* = 21; spGetProcAddress* = 22;
	sfADR* = 23; sfSIZE* = 24; sfBIT* = 25; sfVAL* = 26;
	
	(* Win32 specifics *)
	HeapHandle* = -64;
	ExitProcess* = -56;
	LoadLibraryW_adr* = -48;
	GetProcAddress_adr* = -40;
	GetProcessHeap* = -32;
	HeapAlloc* = -24;
	HeapFree* = -16;
	
TYPE
	IdStr* = S.IdStr;
	String* = S.Str;
	ModuleKey* = ARRAY 2 OF INTEGER;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
    Node* = POINTER TO NodeDesc;
    Ident* = POINTER TO IdentDesc;
	
	TypeList* = POINTER TO RECORD
		type*: Type; a*: INTEGER; next*: TypeList
	END;
	
	ObjDesc* = EXTENSIBLE RECORD
		class*: INTEGER; type*: Type; ident*: Ident;
		regUsed*, xRegUsed*: SET (* for Generator usage *)
	END;
	Const* = POINTER TO EXTENSIBLE RECORD (ObjDesc) val*: INTEGER END;
	Field* = POINTER TO EXTENSIBLE RECORD (ObjDesc) off*: INTEGER END;
	Var* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, expno*, lev*: INTEGER; ronly*: BOOLEAN
	END;
	Par* = POINTER TO EXTENSIBLE RECORD (Var)
		varpar*: BOOLEAN
	END;
	Proc* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, expno*, lev*, locblksize*: INTEGER;
        decl*: Ident; statseq*: Node; return*: Object
	END;
	Str* = POINTER TO EXTENSIBLE RECORD (Var)
		bufpos*, len*: INTEGER
	END;
	Module* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		path*: String; name*: IdStr; 
		key*: ModuleKey; lev*, adr*: INTEGER;
		first*, impList*: Ident; types*: TypeList
	END;
	SProc* = POINTER TO EXTENSIBLE RECORD (ObjDesc) id*: INTEGER END;
	
	IdentDesc* = RECORD
		export*: BOOLEAN;
		name*: IdStr; obj*: Object;
		next*: Ident
	END;
	
	Scope* = POINTER TO RECORD first*: Ident; dsc*: Scope END;
	
	NodeDesc* = EXTENSIBLE RECORD (ObjDesc)
		op*: INTEGER; left*, right*: Object; ronly*: BOOLEAN;
		srcPos*: INTEGER (* for debugging *)
	END;
	
	TypeDesc* = RECORD
		form*, size*, align*, nptr*: INTEGER;
		len*, adr*, lev*, expno*: INTEGER;
		base*: Type; fields*: Ident;
		parblksize*, nfpar*: INTEGER; obj*: Object;
		ref*: INTEGER (* import/export *)
	END;

VAR
	(* Predefined Types *)
	intType*, byteType*, realType*: Type;
	boolType*, setType*, charType*, nilType*, strType*: Type;
	noType*: Type; predefinedTypes: ARRAY 32 OF Type;
	
	LoadLibraryW*, GetProcAddress*: Proc;
	
	topScope*, universe*, systemScope: Scope;
	curLev*, modlev*: INTEGER; modid*: IdStr; modkey*: ModuleKey;
	expList*, strList*: Ident; recList*: TypeList;
	
	symfile: Sys.File;
	refno, preTypeNo, expno*, modno*: INTEGER;
	impTypes: ARRAY MaxExpTypes OF Type;
	modList*: ARRAY MaxImpMod OF Module;
	
	strbuf*: ARRAY 100000H OF CHAR; strbufSize*: INTEGER;
	
	CplFlag* : RECORD
		main*, console*, debug*: BOOLEAN
	END;
	
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

PROCEDURE AppendStr*(ext: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
	VAR i, k: INTEGER;
BEGIN i := 0; WHILE dst[i] # 0X DO INC(i) END;
	k := 0; WHILE ext[k] # 0X DO dst[i+k] := ext[k]; INC(k) END;
	dst[i+k] := 0X
END AppendStr;

PROCEDURE SetCompilerFlag*(pragma: ARRAY OF CHAR);
BEGIN
	IF pragma = 'MAIN' THEN CplFlag.main := TRUE
	ELSIF pragma = 'CONSOLE' THEN
		CplFlag.main := TRUE; CplFlag.console := TRUE
	ELSIF pragma = 'DEBUG' THEN CplFlag.debug := TRUE
	END
END SetCompilerFlag;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE IsOpenArray*(tp: Type): BOOLEAN;
	RETURN (tp.form = tArray) & (tp.len < 0)
END IsOpenArray;

PROCEDURE IsNormalArray*(tp: Type): BOOLEAN;
	RETURN (tp.form = tArray) & (tp.len >= 0)
END IsNormalArray;

PROCEDURE IsStr*(t: Type): BOOLEAN;
	RETURN (t = strType) OR (t.form = tArray) & (t.base.form = tChar)
END IsStr;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewVar*(tp: Type): Var;
	VAR v: Var;
BEGIN
	NEW(v); v.class := cVar; v.ronly := FALSE;
	v.type := tp; v.lev := curLev;
	RETURN v
END NewVar;

PROCEDURE NewConst*(tp: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); c.class := cConst;
	c.type := tp; c.val := val;
	RETURN c
END NewConst;

PROCEDURE NewPar*(proc, tp: Type; varpar: BOOLEAN): Par;
	VAR p: Par;
BEGIN
	NEW(p); p.class := cVar;
	p.type := tp; p.lev := curLev;
	p.varpar := varpar; INC(proc.nfpar);
	p.ronly := ~varpar & (tp.form IN {tArray, tRec});
	RETURN p
END NewPar;

PROCEDURE NewField*(rec, tp: Type): Field;
	VAR fld: Field;
BEGIN
	NEW(fld); fld.class := cField;
	fld.type := tp; rec.nptr := rec.nptr + tp.nptr;
	RETURN fld
END NewField;

PROCEDURE NewStr*(str: String; slen: INTEGER): Str;
	VAR x: Str; i: INTEGER; p: Ident;
BEGIN
	NEW(x); x.class := cVar; x.ronly := TRUE;
	x.type := strType; x.lev := curLev; x.len := slen;
	IF x.lev >= -1 (* need to alloc buffer *) THEN 
		IF strbufSize + slen >= LEN(strbuf) THEN
			S.Mark('too many strings'); x.bufpos := -1
		ELSE x.bufpos := strbufSize; strbufSize := strbufSize + slen;
			FOR i := 0 TO slen-1 DO strbuf[x.bufpos+i] := str[i] END;
			NEW(p); p.obj := x; p.next := strList; strList := p
		END
	ELSE x.bufpos := -1
	END;
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
	NEW(x); x.class := cType;
	x.type := tp; IF tp.obj = NIL THEN tp.obj := x END;
	RETURN x
END NewTypeObj;

PROCEDURE NewSProc*(id, cls: INTEGER): SProc;
	VAR x: SProc;
BEGIN
	NEW(x); x.class := cls;
	x.id := id; x.type := noType;
	RETURN x
END NewSProc;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE NewType*(VAR typ: Type; form: INTEGER);
BEGIN
	NEW(typ); typ.form := form; typ.nptr := 0;
	typ.size := 0; typ.align := 0;
	typ.lev := curLev; typ.ref := -1
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
	VAR tp: Type; p: TypeList;
BEGIN
	NewType(tp, tRec); tp.len := 0;
	IF curLev >= 0 THEN
		NEW(p); p.type := tp; p.next := recList; recList := p
	ELSIF curLev = -1 THEN ASSERT(FALSE)
	END;
	RETURN tp
END NewRecord;

PROCEDURE ExtendRecord*(recType: Type);
BEGIN
	IF recType.base # NIL THEN
		recType.len := recType.base.len + 1;
		recType.nptr := recType.base.nptr
	END
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
	NewType(typ, form); INC(preTypeNo); typ.ref := preTypeNo;
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

PROCEDURE Enter(x: Object; name: IdStr);
	VAR ident: Ident;
BEGIN
	NEW(ident); ident.name := name; ident.export := FALSE;
	ident.obj := x; x.ident := ident;
	ident.next := topScope.first; topScope.first := ident
END Enter;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Export symbol file *)

PROCEDURE ModByLev*(lev: INTEGER): Module;
	RETURN modList[-lev-2]
END ModByLev;

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
		WriteInt(symfile, typ.lev); WriteInt(symfile, typ.ref);
		IF typ.lev < -1 THEN Sys.WriteStr(symfile, modList[-typ.lev-2].name)
		ELSIF typ.lev = 0 THEN
			IF typ.ref < 0 THEN ExportType0(typ) END
		ELSE ASSERT(typ.lev = -1)
		END
	ELSE WriteInt(symfile, -1); WriteInt(symfile, 0)
	END
END DetectType;

PROCEDURE ExportProc(typ: Type);
	VAR par: Ident; x: Par;
BEGIN
	WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
	WriteInt(symfile, typ.parblksize); DetectType(typ.base);
	par := typ.fields;
	WHILE par # NIL DO x := par.obj(Par);
		WriteInt(symfile, x.class);
		Sys.WriteStr(symfile, par.name);
		WriteInt(symfile, ORD(x.varpar));
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
		WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
		DetectType(typ.base); fld := typ.fields;
		WHILE fld # NIL DO
			WriteInt(symfile, cField);
			IF ~fld.export THEN s[0] := 0X; Sys.WriteStr(symfile, s)
			ELSE Sys.WriteStr(symfile, fld.name)
			END;
			DetectType(fld.obj.type);
			WriteInt(symfile, fld.obj(Field).off);
			fld := fld.next
		END;
		WriteInt (symfile, cType)
	ELSIF typ.form = tArray THEN
		WriteInt(symfile, typ.len);
		WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
		DetectType (typ.base)
	ELSIF typ.form = tPtr THEN
		WriteInt(symfile, typ.size); WriteInt(symfile, typ.align);
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
		filename: String;
BEGIN
	refno := 0; expno := 0;
	Sys.Rewrite(symfile, 'sym.temp_'); Sys.Seek(symfile, 16);
	WriteInt (symfile, modlev);
	
	FOR i := 0 TO modno-1 DO
		mod := modList[i];
		WriteInt(symfile, cModule);
		Sys.WriteStr(symfile, mod.name);
		WriteModkey(mod.key)
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
				WriteInt(symfile, expno);
				DetectType(ident.obj.type);
				IF ident.obj.type = strType THEN
					WriteInt(symfile, ident.obj(Str).len)
				END
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
		REPEAT Sys.Read1(symfile, n); chunk[k] := n; INC(i); INC(k)
		UNTIL (i = size) OR (k = 64);
		Crypt.MD5ComputeChunk(hash, SYSTEM.ADR(chunk), k)
	UNTIL i = size;
	
	Sys.Seek(symfile, 0);
	modkey[0] := Crypt.MD5GetLowResult(hash);
	modkey[1] := Crypt.MD5GetHighResult(hash);
	WriteModkey(modkey);
	Sys.Close(symfile);
	
	IF S.errcnt = 0 THEN filename[0] := 0X;
		AppendStr(modid, filename); AppendStr('.sym', filename);
		Sys.Delete(filename); Sys.Rename('sym.temp_', filename)
	ELSE Sys.Delete('sym.temp_')
	END
END WriteSymfile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)

PROCEDURE FindModule(modname: IdStr): Module;
	VAR i: INTEGER; module: Module;
BEGIN module := NIL;
	FOR i := 0 TO modno-1 DO
		IF modList[i].name = modname THEN module := modList[i] END
	END;
	RETURN module
END FindModule;

PROCEDURE NewImportIdent(ident: Ident; name: IdStr; x: Object): Ident;
BEGIN
	IF ident # NIL THEN NEW(ident.next); ident := ident.next
	ELSE NEW(topScope.first); ident := topScope.first
	END;
	ident.export := FALSE; ident.obj := x; ident.name := name;
	IF x.ident = NIL THEN x.ident := ident END;
	RETURN ident
END NewImportIdent;

PROCEDURE DetectTypeI(VAR typ: Type);
	VAR mod, ref, i: INTEGER; modname: IdStr; msg: String;
		module: Module; p: TypeList;
BEGIN
	ReadInt(symfile, mod); ReadInt(symfile, ref);
	IF mod = -1 THEN typ := predefinedTypes[ref]
	ELSIF mod = 0 THEN
		IF ref >= 0 THEN
			i := -(curLev+2); p := modList[i].types;
			WHILE (p # NIL) & (p.type.ref # ref) DO p := p.next END;
			typ := p.type
		ELSE ImportType0(typ)
		END
	ELSIF mod < -1 THEN
		Sys.ReadStr(symfile, modname); module := FindModule(modname);
		IF module # NIL THEN p := module.types;
			WHILE (p # NIL) & (p.type.ref # ref) DO p := p.next END;
			typ := p.type
		ELSE msg := 'Need to import '; AppendStr(modname, msg);
			AppendStr(' in order to import ', msg); i := -(curLev+2);
			AppendStr(modList[i].name, msg); S.Mark(msg)
		END
	ELSE ASSERT(FALSE)
	END
END DetectTypeI;

PROCEDURE AddToTypeList(typ: Type);
	VAR i: INTEGER; p: TypeList;
BEGIN i := -(curLev+2);
	IF modList[i].types # NIL THEN p := modList[i].types;
		WHILE p.next # NIL DO p := p.next END;
		NEW(p.next); p := p.next; p.type := typ
	ELSE NEW(modList[i].types); modList[i].types.type := typ
	END
END AddToTypeList;

PROCEDURE ImportProc(VAR typ: Type; ref: INTEGER);
	VAR par: Ident; x: Par; xtype: Type;
		cls, n: INTEGER; name: IdStr; varpar: BOOLEAN;
BEGIN typ := NewProcType();
	IF ref > -1 THEN typ.ref := ref; AddToTypeList(typ) END;
	ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
	ReadInt(symfile, typ.parblksize); DetectTypeI(typ.base);
	ReadInt(symfile, cls); OpenScope;
	WHILE cls # cType DO
		Sys.ReadStr(symfile, name);
		ReadInt(symfile, n); varpar := n = ORD(TRUE);
		DetectTypeI(xtype); x := NewPar(typ, xtype, varpar);
		par := NewImportIdent(par, name, x);
		ReadInt(symfile, cls)
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
		typ := NewRecord(); AddToTypeList(typ);
		typ.ref := ref; typ.expno := exp; typ.adr := 0;
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		DetectTypeI(typ.base); ExtendRecord(typ);
		ReadInt(symfile, cls); OpenScope;
		WHILE cls # cType DO
			Sys.ReadStr(symfile, name);
			DetectTypeI(fltype);
			x := NewField(typ, fltype);
			ReadInt(symfile, x(Field).off);
			fld := NewImportIdent(fld, name, x);
			ReadInt(symfile, cls)
		END;
		typ.fields := topScope.first; CloseScope
	ELSIF form = tArray THEN
		ReadInt(symfile, len); typ := NewArray(len); 
		typ.ref := ref; AddToTypeList(typ);
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		DetectTypeI(typ.base); CompleteArray(typ)
	ELSIF form = tPtr THEN
		typ := NewPointer(); typ.ref := ref; AddToTypeList(typ);
		ReadInt(symfile, typ.size); ReadInt(symfile, typ.align);
		DetectTypeI(typ.base)
	ELSIF form = tProc THEN
		ImportProc(typ, ref)
	END
END ImportType;

PROCEDURE ReadModkey(VAR key: ModuleKey);
BEGIN
	Sys.Read8(symfile, key[0]);
	Sys.Read8(symfile, key[1])
END ReadModkey;

PROCEDURE ImportModules*;
	VAR ident: Ident; x: Object; cls, i, j, k, val, slen: INTEGER;
		module, dep: Module; name, depname: IdStr; tp: Type;
		depkey: ModuleKey; msg: String;
BEGIN
	(* Sort module list *)
	FOR i := 0 TO modno-2 DO k := i;
		FOR j := i+1 TO modno-1 DO
			IF modList[k].lev > modList[j].lev THEN k := j END
		END;
		IF i # k THEN module := modList[i];
			modList[i] := modList[k]; modList[k] := module
		END
	END;
	FOR i := 0 TO modno-1 DO
		module := modList[i]; Sys.Open(symfile, module.path);
		ReadModkey(module.key); ReadInt(symfile, module.lev);
		
		OpenScope; curLev := -(i+2); ident := NIL;
		ReadInt(symfile, cls);
		WHILE cls # cNull DO
			IF cls = cConst THEN
				Sys.ReadStr(symfile, name);
				ReadInt(symfile, val);
				DetectTypeI(tp); x := NewConst(tp, val);
				ident := NewImportIdent(ident, name, x)
			ELSIF cls = cType THEN
				Sys.ReadStr(symfile, name);
				DetectTypeI(tp); x := NewTypeObj(tp);
				ident := NewImportIdent(ident, name, x)
			ELSIF cls = cVar THEN
				Sys.ReadStr(symfile, name);
				ReadInt(symfile, val); DetectTypeI(tp);
				IF tp # strType THEN x := NewVar(tp); x(Var).ronly := TRUE
				ELSE ReadInt(symfile, slen); x := NewStr('', slen)
				END;
				x(Var).expno := val; x(Var).adr := 0;
				ident := NewImportIdent(ident, name, x)
			ELSIF cls = cProc THEN
				Sys.ReadStr(symfile, name);
				x := NewProc(); ReadInt(symfile, x(Proc).expno);
				x(Proc).adr := 0; ImportProc(x.type, -1);
				ident := NewImportIdent(ident, name, x)
			ELSIF cls = cModule THEN
				Sys.ReadStr(symfile, name); ReadModkey(depkey);
				dep := FindModule(name);
				IF name = modid THEN S.Mark('Circular dependency')
				ELSIF dep # NIL THEN
					IF (dep.key[0] # depkey[0]) OR (dep.key[1] # depkey[1])
					THEN msg := 'Module '; AppendStr(name, msg);
						AppendStr(' was imported by ', msg);
						AppendStr(module.name, msg);
						AppendStr(' with a different key', msg); S.Mark(msg)
					END
				END
			END;
			ReadInt(symfile, cls)
		END;
		module.first := topScope.first; CloseScope
	END;
	Sys.Close(symfile); curLev := 0
END ImportModules;

PROCEDURE NewModule*(modident: Ident; modname: IdStr);
	VAR path: String; module: Module;
BEGIN
	path[0] := 0X; AppendStr(modname, path); AppendStr('.sym', path);
	IF modname = 'SYSTEM' THEN
		NEW(module); module.name := modname;
		module.lev := -1; module.first := systemScope.first;
		IF modident # NIL THEN
			modident.obj := module; module.ident := modident
		END
	ELSIF Sys.Existed(path) THEN
		NEW(module); module.name := modname;
		module.path := path; module.lev := 0; module.adr := 0;
		IF modident # NIL THEN
			modident.obj := module; module.ident := modident
		END;
		modList[modno] := module; INC(modno); Sys.Open(symfile, path);
		ReadModkey(module.key); ReadInt(symfile, module.lev);
		IF module.lev >= modlev THEN modlev := module.lev + 1;
			IF modlev > MaxModLev THEN S.Mark('Module level too deep') END
		END;
		Sys.Close(symfile)
	ELSE S.Mark('Symbol file not existed')
	END;
END NewModule;

PROCEDURE Init*(modname: IdStr);
BEGIN
	NEW(universe); topScope := universe; curLev := -1;
	modid := modname; modno := 0; strbufSize := 0;
	
	Enter(NewTypeObj(intType), 'INTEGER');
	Enter(NewTypeObj(byteType), 'BYTE');
	Enter(NewTypeObj(realType), 'REAL');
	Enter(NewTypeObj(setType), 'SET');
	Enter(NewTypeObj(boolType), 'BOOLEAN');
	Enter(NewTypeObj(charType), 'CHAR');
	
	Enter(NewSProc(spINC, cSProc), 'INC');
	Enter(NewSProc(spDEC, cSProc), 'DEC');
	Enter(NewSProc(spINCL, cSProc), 'INCL');
	Enter(NewSProc(spEXCL, cSProc), 'EXCL');
	Enter(NewSProc(spNEW, cSProc), 'NEW');
	Enter(NewSProc(spASSERT, cSProc), 'ASSERT');
	Enter(NewSProc(spPACK, cSProc), 'PACK');
	Enter(NewSProc(spUNPK, cSProc), 'UNPK');
	
	Enter(NewSProc(sfABS, cSFunc), 'ABS');
	Enter(NewSProc(sfODD, cSFunc), 'ODD');
	Enter(NewSProc(sfLEN, cSFunc), 'LEN');
	Enter(NewSProc(sfLSL, cSFunc), 'LSL');
	Enter(NewSProc(sfASR, cSFunc), 'ASR');
	Enter(NewSProc(sfROR, cSFunc), 'ROR');
	Enter(NewSProc(sfFLOOR, cSFunc), 'FLOOR');
	Enter(NewSProc(sfFLT, cSFunc), 'FLT');
	Enter(NewSProc(sfORD, cSFunc), 'ORD');
	Enter(NewSProc(sfCHR, cSFunc), 'CHR');
	
	OpenScope;
	Enter(NewSProc(spGET, cSProc), 'GET');
	Enter(NewSProc(spPUT, cSProc), 'PUT');
	Enter(NewSProc(spCOPY, cSProc), 'COPY');
	Enter(NewSProc(spLoadLibraryW, cSProc), 'LoadLibraryW');
	Enter(NewSProc(spGetProcAddress, cSProc), 'GetProcAddress');
	Enter(NewSProc(S.spINT3, cSProc), 'INT3');
	
	Enter(NewSProc(sfADR, cSFunc), 'ADR');
	Enter(NewSProc(sfSIZE, cSFunc), 'SIZE');
	Enter(NewSProc(sfBIT, cSFunc), 'BIT');
	Enter(NewSProc(sfVAL, cSFunc), 'VAL');
	
	Enter(NewTypeObj(byteType), 'BYTE');
	systemScope := topScope; CloseScope;
	
	LoadLibraryW := NewProc(); LoadLibraryW.type := NewProcType();
	OpenScope; Enter(NewPar(LoadLibraryW.type, strType, FALSE), '1');
	LoadLibraryW.type.fields := topScope.first; CloseScope;
	LoadLibraryW.type.parblksize := 8;
	LoadLibraryW.type.base := intType;
	LoadLibraryW.adr := -48;
	
	GetProcAddress := NewProc(); GetProcAddress.type := NewProcType();
	OpenScope; Enter(NewPar(GetProcAddress.type, intType, FALSE), '2');
	Enter(NewPar(GetProcAddress.type, intType, FALSE), '1');
	GetProcAddress.type.fields := topScope.first; CloseScope;
	GetProcAddress.type.parblksize := 16;
	GetProcAddress.type.base := intType;
	GetProcAddress.adr := -40;
	
	curLev := 0
END Init;

BEGIN
	ExportType0 := ExportType; ImportType0 := ImportType;
	S.InstallSetCompilerFlag(SetCompilerFlag);

	preTypeNo := 0; predefinedTypes[0] := NIL; curLev := -1;
	NewPredefinedType(intType, tInt);
	NewPredefinedType(byteType, tInt);
	NewPredefinedType(boolType, tBool);
	NewPredefinedType(setType, tSet);
	NewPredefinedType(charType, tChar);
	NewPredefinedType(nilType, tNil);
	NewPredefinedType(realType, tReal);
	NewPredefinedType(strType, tStr);
	NewPredefinedType(noType, tNull)
END Base.