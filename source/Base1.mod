MODULE Base1;

IMPORT
	SYSTEM, Kernel32, Console;

CONST
	WordSize* = 8; CharSize* = 2; MaxChar* = 65535; MaxSet* = 64;
	MaxInt* = 9223372036854775807; MinInt* = -MaxInt - 1;
	MaxIdLen* = 63; MaxStrLen* = 255;
	MaxExt* = 8; MaxRecTypes* = 512;
	MaxImpMod* = 256; MaxExpTypes* = 1024;
	
	(* Type form *)
	tInt* = 0; tBool* = 1; tSet* = 2; tChar* = 3; tReal* = 4;
	tPtr* = 5; tProc* = 6; tArray* = 7; tRec* = 8; tStr* = 9; tNil* = 10;
	
	typEql* = {tBool, tSet, tPtr, tProc, tNil};
	typCmp* = {tInt, tReal, tChar, tStr};

TYPE
	FileHandle* = RECORD handle: Kernel32.HANDLE END;
	IdStr* = ARRAY MaxIdLen+1 OF CHAR;
	String* = ARRAY MaxStrLen+1 OF CHAR;
	
	Type* = POINTER TO TypeDesc;
	Object* = POINTER TO ObjDesc;
    Node* = POINTER TO NodeDesc;
    Ident* = POINTER TO IdentDesc;
	
	ObjDesc* = EXTENSIBLE RECORD isType*: BOOLEAN; type*: Type END;
	Const* = POINTER TO EXTENSIBLE RECORD (ObjDesc) val*: INTEGER END;
	Field* = POINTER TO EXTENSIBLE RECORD (ObjDesc) off*: INTEGER END;
	Var* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, lev*: INTEGER; ref*, ronly*: BOOLEAN
	END;
	Proc* = POINTER TO EXTENSIBLE RECORD (ObjDesc)
		adr*, lev*, locblksize*: INTEGER; ref*: BOOLEAN;
        decl*: Ident; statseq*: Node; return*: Object
	END;
	
	IdentDesc* = RECORD name*: IdStr; obj*: Object; next*: Ident END;
	Scope* = POINTER TO RECORD first*: Ident; dsc*: Scope END;
	
	NodeDesc* = EXTENSIBLE RECORD (ObjDesc)
		op*: INTEGER; left*, right*: Object
	END;
	
	TypeDesc* = RECORD
		form*, size*, align*: INTEGER;
		len*: INTEGER; base*: Type; fields*: Ident;
		parblksize*, nfpar*: INTEGER
	END;

VAR
	(* Predefined Types *)
	intType*, byteType*, realType*: Type;
	boolType*, setType*, charType*, nilType*: Type;
	
	topScope*, universe*: Scope;
	curLev*: INTEGER;
	
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

PROCEDURE Write_string* (VAR file: FileHandle; str: ARRAY OF CHAR);
	VAR i: INTEGER;
BEGIN i := 0;
	WHILE (i < LEN(str)) & (str[i] # 0X) DO
		Write_2bytes (file, ORD(str[i])); INC (i)
	END;
	Write_2bytes (file, 0)
END Write_string;
	
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

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
	
PROCEDURE NewVar*(tp: Type; VAR varblksize: INTEGER): Var;
	VAR v: Var; adr: INTEGER;
BEGIN
	NEW(v); v.isType := FALSE; v.type := tp; v.ref := FALSE; v.ronly := FALSE;
	adr := -varblksize - tp.size; adr := adr - adr MOD tp.align;
	v.adr := adr; v.lev := curLev; varblksize := -adr;
	RETURN v
END NewVar;

PROCEDURE NewConst*(tp: Type; val: INTEGER): Const;
	VAR c: Const;
BEGIN
	NEW(c); c.isType := FALSE; c.type := tp; c.val := val;
	RETURN c
END NewConst;

PROCEDURE NewPar*(proc: Type; ref, ronly: BOOLEAN; tp: Type): Var;
	VAR v: Var; parsize: INTEGER;
BEGIN
	IF ~ref & (tp.form IN {tArray, tRec}) THEN
		ref := (tp.size # 1) & (tp.size # 2) & (tp.size # 4) & (tp.size # 8)
	END;
	IF ref & ~ronly & (tp.form = tRec)
	OR (tp.form = tArray) & (tp.len = 0) THEN
		parsize := WordSize * 2
	ELSE parsize := WordSize
	END;
	NEW(v); v.isType := FALSE; v.type := tp;
	v.adr := proc.parblksize; v.ref := ref; v.ronly := ronly;
	proc.parblksize := proc.parblksize + parsize; INC(proc.nfpar);
	RETURN v
END NewPar;

PROCEDURE NewField*(rec, tp: Type): Field;
	VAR fld: Field; off: INTEGER;
BEGIN
	NEW(fld); fld.isType := FALSE; fld.type := tp;
	off := rec.size; off := off + (-off) MOD tp.align;
	fld.off := off; rec.size := off + tp.size;
	IF rec.align < tp.align THEN rec.align := tp.align END
END NewField;

PROCEDURE NewStr*(str: String; slen: INTEGER): Var;
	RETURN NIL
END NewStr;

PROCEDURE NewProc*(): Proc;
	VAR p: Proc;
BEGIN
	NEW(p); p.isType := FALSE; p.lev := curLev; p.ref := FALSE
	RETURN p
END NewProc;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InitNewType(tp: Type);
BEGIN tp.align := 0; tp.size := 0
END InitNewType;

PROCEDURE NewArray*(len: INTEGER): Type;
	VAR tp: Type;
BEGIN
	NEW(tp); InitNewType(tp); tp.form := tArray; tp.len := len;
	RETURN tp
END NewArray;

PROCEDURE CalculateArraySize*(arrType, lastArray: Type);
BEGIN
	IF arrType # lastArray THEN
		CalculateArraySize(arrType.base, lastArray)
	END;
	arrType.size := arrType.len * arrType.base.size;
	IF arrType.align < arrType.base.align THEN
		arrType.align := arrType.base.align
	END
END CalculateArraySize;

PROCEDURE NewRecord*(): Type;
	VAR tp: Type;
BEGIN
	NEW(tp); InitNewType(tp); tp.form := tRec; tp.len := 0;
	RETURN tp
END NewRecord;

PROCEDURE ExtendRecord*(recType: Type);
BEGIN
	recType.size := recType.base.size;
	recType.align := recType.base.align;
	recType.len := recType.base.len + 1
END ExtendRecord;

PROCEDURE NewPointer*(): Type;
	VAR tp: Type;
BEGIN
	NEW(tp); InitNewType(tp);
	tp.form := tPtr; tp.size := WordSize; tp.align := WordSize;
	RETURN tp
END NewPointer;

PROCEDURE NewProcType*(): Type;
	VAR tp: Type;
BEGIN
	NEW(tp); InitNewType(tp); tp.form := tProc;
	tp.size := WordSize; tp.align := WordSize;
	tp.parblksize := 0; tp.nfpar := 0;
	RETURN tp
END NewProcType;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE OpenScope*;
	VAR scp: Scope;
BEGIN NEW(scp); scp.dsc := topScope; topScope := scp
END OpenScope;

PROCEDURE CloseScope*;
BEGIN topScope := topScope.dsc
END CloseScope;

BEGIN
	NEW(universe); topScope := universe; curLev := 0
END Base1.