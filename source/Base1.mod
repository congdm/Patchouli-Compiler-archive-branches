MODULE Base1;

IMPORT
	SYSTEM, Kernel32, Console, Crypt;

CONST
	MaxIdLen* = 63; MaxStrLen* = 255;
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
	FileHandle* = RECORD handle: Kernel32.HANDLE END;
	IdStr* = ARRAY MaxIdLen+1 OF CHAR;
	String* = ARRAY MaxStrLen+1 OF CHAR;
	ModuleKey* = ARRAY 2 OF INTEGER;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
    Node* = POINTER TO NodeDesc;
    Ident* = POINTER TO IdentDesc;
	
	ObjDesc* = EXTENSIBLE RECORD class*: INTEGER; type*: Type END;
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
		export*: BOOLEAN;
		name*: IdStr; key*: ModuleKey;
		lev*: INTEGER; first*: Ident
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
		parblksize*, nfpar*: INTEGER;
		mod*, ref*: INTEGER (* import/export *)
	END;

VAR
	(* Predefined Types *)
	intType*, byteType*, realType*, longrealType*: Type;
	boolType*, setType*, charType*, nilType*, strType*: Type;
	noType*: Type; predefinedTypes: ARRAY 32 OF Type;
	
	topScope*, universe*: Scope;
	curLev*, modlev*: INTEGER;
	modkey*: ModuleKey;
	
	symfile: FileHandle;
	refno, preTypeNo, expno, nmod: INTEGER;
	expList*: Ident;
	modList*: ARRAY MaxImpMod OF Module;
	
	ExportType0: PROCEDURE(typ: Type);
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Strings *)
	
PROCEDURE StrEqual* (s1, s2: ARRAY OF CHAR) : BOOLEAN;
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(s1)) & (i < LEN(s2)) & (s1[i] # 0X) & (s1[i] = s2[i]) DO
		INC (i)
	END;
	RETURN (i < LEN(s1)) & (i < LEN(s2)) & (s1[i] = s2[i])
		OR (LEN(s1) = LEN(s2)) & (i = LEN(s1))
		OR (i = LEN(s2)) & (s1[i] = 0X)
		OR (i = LEN(s1)) & (s2[i] = 0X)
END StrEqual;

PROCEDURE StrCopy* (src: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN
	i := 0;
	WHILE (i < LEN(dst) - 1) & (i < LEN(src)) & (src[i] # 0X) DO
		dst[i] := src[i]; INC(i)
	END;
	dst[i] := 0X
END StrCopy;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* System functions wrappers *)

PROCEDURE File_existed* (filename: ARRAY OF CHAR): BOOLEAN;
	VAR attr: CARD32;
BEGIN attr := Kernel32.GetFileAttributesW(filename);
	RETURN attr # ORD(Kernel32.INVALID_FILE_ATTRIBUTES)
END File_existed;
	
PROCEDURE Open* (VAR file: FileHandle; filename: ARRAY OF CHAR);
BEGIN
	IF File_existed(filename) THEN
		file.handle := Kernel32.CreateFileW(
			filename, ORD(Kernel32.GENERIC_READ + Kernel32.GENERIC_WRITE),
			0, NIL, Kernel32.OPEN_EXISTING, 0, 0
		)
	ELSE Console.WriteString ('File not existed!'); Console.WriteLn
	END
END Open;
	
PROCEDURE Rewrite* (VAR file: FileHandle; filename: ARRAY OF CHAR);
BEGIN
	file.handle := Kernel32.CreateFileW(
		filename, ORD(Kernel32.GENERIC_READ + Kernel32.GENERIC_WRITE),
		0, NIL, Kernel32.CREATE_ALWAYS, 0, 0
	)
END Rewrite;

PROCEDURE Close* (VAR file : FileHandle);
	VAR bRes: Kernel32.BOOL;
BEGIN
	IF file.handle # 0 THEN
		bRes := Kernel32.CloseHandle (file.handle); file.handle := 0
	END
END Close;

PROCEDURE Rename_file* (oldname, newname: ARRAY OF CHAR);
	VAR bRes: Kernel32.BOOL;
BEGIN
	bRes := Kernel32.MoveFileW (oldname, newname)
END Rename_file;

PROCEDURE Delete_file* (filename : ARRAY OF CHAR);
	VAR bRes: Kernel32.BOOL;
BEGIN
	bRes := Kernel32.DeleteFileW (filename)
END Delete_file;

(* -------------------------------------------------------------------------- *)

PROCEDURE Read_byte* (VAR file: FileHandle; VAR n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf: BYTE; byteRead: CARD32;
BEGIN
	bRes := Kernel32.ReadFile (file.handle, buf, 1, byteRead, NIL);
	IF (bRes = 0) OR (byteRead # 1) THEN n := -1 ELSE n := buf END
END Read_byte;
	
PROCEDURE Read_2bytes* (VAR file: FileHandle; VAR n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf: CARD16; byteRead: CARD32;
BEGIN
	bRes := Kernel32.ReadFile (file.handle, buf, 2, byteRead, NIL);
	IF (bRes = 0) OR (byteRead # 2) THEN n := -1 ELSE n := buf END
END Read_2bytes;

PROCEDURE Read_string* (VAR file: FileHandle; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC (i); Read_2bytes (file, n); str[i] := CHR(n)
	UNTIL n = 0
END Read_string;
	
PROCEDURE Read_4bytes* (VAR file: FileHandle; VAR n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf, byteRead: CARD32;
BEGIN
	bRes := Kernel32.ReadFile (file.handle, buf, 4, byteRead, NIL);
	IF (bRes = 0) OR (byteRead # 4) THEN n := -1 ELSE n := buf END
END Read_4bytes;
	
PROCEDURE Read_8bytes* (VAR file : FileHandle; VAR n : INTEGER);
	VAR bRes: Kernel32.BOOL; buf: INTEGER; byteRead: CARD32;
BEGIN
	bRes := Kernel32.ReadFile (file.handle, buf, 8, byteRead, NIL);
	IF (bRes = 0) OR (byteRead # 8) THEN n := -1 ELSE n := buf END
END Read_8bytes;

PROCEDURE Read_bytes* (
	VAR file: FileHandle; VAR buf: ARRAY OF SYSTEM.BYTE; VAR byteRead: INTEGER
);
	VAR bRes: Kernel32.BOOL; bRead: CARD32;
BEGIN
	bRes := Kernel32.ReadFile (file.handle, buf, LEN(buf), bRead, NIL);
	byteRead := bRead
END Read_bytes;

(* -------------------------------------------------------------------------- *)
	
PROCEDURE Write_byte* (VAR file: FileHandle; n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf: BYTE; byteWritten: CARD32;
BEGIN buf := n;
	bRes := Kernel32.WriteFile (file.handle, buf, 1, byteWritten, NIL)
END Write_byte;

PROCEDURE Write_ansi_str* (VAR file: FileHandle; str: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE (i < LEN(str)) & (str[i] # 0X) DO
		Write_byte (file, ORD(str[i])); INC (i)
	END;
	Write_byte (file, 0)
END Write_ansi_str;
	
PROCEDURE Write_2bytes* (VAR file: FileHandle; n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf: CARD16; byteWritten: CARD32;
BEGIN buf := n;
	bRes := Kernel32.WriteFile (file.handle, buf, 2, byteWritten, NIL)
END Write_2bytes;

PROCEDURE WriteStr* (VAR file: FileHandle; str: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE (i < LEN(str)) & (str[i] # 0X) DO
		Write_2bytes (file, ORD(str[i])); INC (i)
	END;
	Write_2bytes (file, 0)
END WriteStr;
	
PROCEDURE Write_4bytes* (VAR file: FileHandle; n: INTEGER);
	VAR bRes: Kernel32.BOOL; buf, byteWritten: CARD32;
BEGIN buf := n;
	bRes := Kernel32.WriteFile (file.handle, buf, 4, byteWritten, NIL)
END Write_4bytes;
	
PROCEDURE Write_8bytes* (VAR file : FileHandle; n : INTEGER);
	VAR bRes: Kernel32.BOOL; byteWritten: CARD32;
BEGIN
	bRes := Kernel32.WriteFile (file.handle, n, 8, byteWritten, NIL)
END Write_8bytes;

PROCEDURE Write_bytes* (
	VAR file: FileHandle;
	VAR buf: ARRAY OF SYSTEM.BYTE;
	VAR byteWritten: INTEGER
);
	VAR bRes: Kernel32.BOOL; bWritten: CARD32;
BEGIN
	bRes := Kernel32.WriteFile (file.handle, buf, LEN(buf), bWritten, NIL);
	byteWritten := bWritten
END Write_bytes;

PROCEDURE Write_bytes2* (
	VAR file: FileHandle; bufAdr: INTEGER; VAR byteWritten: INTEGER
);
	TYPE ByteArray = ARRAY OF BYTE;
	VAR bRes: Kernel32.BOOL; bWritten: CARD32;
BEGIN
	bRes := Kernel32.WriteFile (
		file.handle, bufAdr{ByteArray}, byteWritten, bWritten, NIL
	);
	byteWritten := bWritten
END Write_bytes2;

PROCEDURE FilePos* (VAR file: FileHandle): INTEGER;
	VAR bRes: Kernel32.BOOL; byteToMove, newPointer: Kernel32.LARGE_INTEGER;
BEGIN byteToMove.QuadPart := 0;
	bRes := Kernel32.SetFilePointerEx(
		file.handle, byteToMove, newPointer, Kernel32.FILE_CURRENT
	);
	RETURN newPointer.QuadPart
END FilePos;

PROCEDURE Seek* (VAR file: FileHandle; pos: INTEGER);
	VAR bRes: Kernel32.BOOL; byteToMove, newPointer: Kernel32.LARGE_INTEGER;
BEGIN byteToMove.QuadPart := pos;
	bRes := Kernel32.SetFilePointerEx(
		file.handle, byteToMove, newPointer, Kernel32.FILE_BEGIN
	)
END Seek;

PROCEDURE SeekRel* (VAR file: FileHandle; offset: INTEGER);
	VAR bRes: Kernel32.BOOL; byteToMove, newPointer: Kernel32.LARGE_INTEGER;
BEGIN byteToMove.QuadPart := offset;
	bRes := Kernel32.SetFilePointerEx(
		file.handle, byteToMove, newPointer, Kernel32.FILE_CURRENT
	)
END SeekRel;

PROCEDURE GetTickCount*() : INTEGER;
	RETURN Kernel32.GetTickCount()
END GetTickCount;

PROCEDURE GetArg* (VAR out: ARRAY OF CHAR; VAR paramLen: INTEGER; n: INTEGER);
	VAR i, k: INTEGER; buf: Kernel32.LPVOID;
BEGIN buf := Kernel32.GetCommandLineW(); i := 0;
	WHILE n > 0 DO
		WHILE (buf{Kernel32.WSTR}[i] # ' ') & (buf{Kernel32.WSTR}[i] # 0X) DO
			INC (i)
		END;
		IF buf{Kernel32.WSTR}[i] = 0X THEN n := 0
		ELSIF buf{Kernel32.WSTR}[i] = ' ' THEN DEC (n);
			WHILE buf{Kernel32.WSTR}[i] = ' ' DO INC (i) END
		END
	END;
	k := 0; paramLen := 0;
	WHILE (buf{Kernel32.WSTR}[i] # ' ') & (buf{Kernel32.WSTR}[i] # 0X) DO
		IF k < LEN(out) THEN out[k] := buf{Kernel32.WSTR}[i] END;
		INC (k); INC (i); INC (paramLen)
	END;
	IF k < LEN(out) THEN out[k] := 0X END
END GetArg;

PROCEDURE WriteInt* (VAR f: FileHandle; n: INTEGER);
	VAR finish: BOOLEAN; b: INTEGER;
BEGIN
	REPEAT b := n MOD 128; finish := (n >= -64) & (n < 64);
		IF finish THEN b := b + 128 ELSE n := n DIV 128 END;
		Write_byte (f, b)
	UNTIL finish
END WriteInt;

PROCEDURE ReadInt* (VAR f: FileHandle; VAR n: INTEGER);
	CONST MaxInt = 9223372036854775807; MinInt = -MaxInt - 1;
	VAR finish: BOOLEAN; i, b, k: INTEGER;
BEGIN n := 0; i := 1; k := 1;
	REPEAT Read_byte (f, b);
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
	NEW(x); x.class := cType; x.type := tp;
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
	ident.obj := x; ident.next := topScope.first; topScope.first := ident
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
		IF typ.mod >= 0 THEN WriteStr(symfile, modList[typ.mod].name) END;
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
		WriteStr(symfile, par.name);
		WriteInt(symfile, ORD(x.ronly));
		DetectType(x.type);
		par := par.next
	END;
	WriteInt(symfile, cType)
END ExportProc;
	
PROCEDURE ExportType(typ: Type);
	VAR fld, ident: Ident; i: INTEGER; s: String;
BEGIN
	IF refno < MaxExpTypes THEN typ.ref := refno; INC(refno)
	ELSE (* stub *)
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
		WriteInt(symfile, typ.len);
		WriteInt(symfile, typ.nptr);
		i := 0; fld := typ.fields;
		WHILE fld # NIL DO
			IF fld.export OR (fld.obj.type.nptr > 0) THEN
				WriteInt(symfile, cField);
				IF ~fld.export THEN s[0] := 0X; WriteStr(symfile, s)
				ELSE WriteStr(symfile, fld.name)
				END;
				DetectType(fld.obj.type)
			END;
			fld := fld.next
		END;
		WriteInt (symfile, cType)
	ELSIF typ.form = tArray THEN
		DetectType (typ.base);
		WriteInt(symfile, typ.len);
		WriteInt(symfile, typ.nptr)
	ELSIF typ.form = tPtr THEN
		DetectType(typ.base)
	ELSIF typ.form = tProc THEN
		ExportProc(typ)
	END
END ExportType;

PROCEDURE WriteModkey(key: ModuleKey);
BEGIN
	Write_8bytes(symfile, key[0]);
	Write_8bytes(symfile, key[1])
END WriteModkey;

PROCEDURE WriteSymfile*;
	VAR ident, exp: Ident; i, k, n, size: INTEGER; mod: Module;
		hash: Crypt.MD5Hash; chunk: ARRAY 64 OF BYTE;
BEGIN
	refno := 0; expno := 0;
	Rewrite(symfile, 'sym.temp_'); Seek(symfile, 16);
	WriteInt (symfile, modlev);
	
	FOR i := 0 TO nmod-1 DO
		mod := modList[0];
		IF mod.export THEN
			WriteInt(symfile, cModule);
			WriteStr(symfile, mod.name);
			WriteModkey(mod.key)
		END
	END;
	
	ident := universe.first;
	WHILE ident # NIL DO
		IF ident.export THEN
			IF ident.obj.class = cConst THEN
				WriteInt(symfile, cConst);
				WriteStr(symfile, ident.name);
				WriteInt(symfile, ident.obj(Const).val);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cType THEN
				WriteInt(symfile, cType);
				WriteStr(symfile, ident.name);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cVar THEN
				WriteInt(symfile, cVar);
				WriteStr(symfile, ident.name);
				NewExport(exp); exp.obj := ident.obj;
				WriteInt (symfile, expno);
				DetectType(ident.obj.type)
			ELSIF ident.obj.class = cProc THEN
				WriteInt(symfile, cProc);
				WriteStr(symfile, ident.name);
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

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Import symbol file *)



PROCEDURE Init*;
BEGIN
	NEW(universe); topScope := universe; curLev := 0; nmod := -1;
	
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
	ExportType0 := ExportType;

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