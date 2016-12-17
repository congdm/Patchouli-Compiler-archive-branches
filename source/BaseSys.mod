MODULE BaseSys;

IMPORT
	SYSTEM;
	
CONST
	memFileBlk = 128;

	Kernel32 = 'Kernel32.dll';
	GENERIC_READ = {31}; GENERIC_WRITE = {30};
	
TYPE
	Pointer = INTEGER;
	Handle = INTEGER;
	Int = INTEGER;
	Uint = INTEGER;
	Dword = INTEGER;
	Word = INTEGER;
	Bool = INTEGER;
	LargeInteger = INTEGER;
	SizeT = INTEGER;
	
	File* = RECORD handle: Handle END;
	MemFile* = POINTER TO RECORD ptr: Pointer; len, maxlen: INTEGER END;
	MemFileRider* = RECORD f: MemFile; pos: INTEGER; eof*: BOOLEAN END;
	
VAR
	GetFileAttributesW: PROCEDURE(lpFileName: Pointer): Dword;
	MoveFileW: PROCEDURE(lpExistingFileName, lpNewFileName: Pointer): Bool;
	DeleteFileW: PROCEDURE(lpFilename: Pointer): Bool;
	CreateFileW: PROCEDURE(
		lpFileName: Pointer;
		dwDesiredAccess, dwShareMode: Dword;
		lpSecurityAttributes: Pointer;
		dwCreationDisposition, dwFlagsAndAttributes: Dword;
		hTemplateFile: Handle
	): Handle;
	CloseHandle: PROCEDURE(hObject: Handle): Bool;
	ReadFile: PROCEDURE(
		hFile: Handle;
		lpBuffer: Pointer;
		nNumberOfBytesToRead: Dword;
		lpNumberOfBytesRead, lpOverlapped: Pointer
	): Bool;
	WriteFile: PROCEDURE(
		hFile: Handle;
		lpBuffer: Pointer;
		nNumberOfBytesToWrite: Dword;
		lpNumberOfBytesWrite, lpOverlapped: Pointer
	): Bool;
	SetFilePointerEx: PROCEDURE(
		hFile: Handle;
		liDistanceToMove: LargeInteger;
		lpNewFilePointer: Pointer;
		dwMoveMethod: Dword
	): Bool;
	GetTickCount_: PROCEDURE(): Dword;
	GetCommandLineW: PROCEDURE(): Pointer;
	WideCharToMultiByte: PROCEDURE(
		CodePage: Uint;
		dwFlags: Dword;
		lpWideCharStr: Pointer;
		cchWideChar: Int;
		lpMultiByteStr: Pointer;
		cbMultiByte: Int;
		lpDefaultChar, lpUsedDefaultChar: Pointer
	): Int;
	GetStdHandle: PROCEDURE(nStdHandle: Dword): Handle;
	GetProcessHeap: PROCEDURE(): Handle;
	HeapAlloc: PROCEDURE(
		hHeap: Handle;
		dwFlags: Dword;
		dwBytes: SizeT
	): Pointer;
	HeapFree: PROCEDURE(
		hHeap: Handle;
		dwFlags: Dword;
		lpMem: Pointer
	): Bool;
	HeapReAlloc: PROCEDURE(
		hHeap: Handle;
		dwFlags: Dword;
		lpMem: Pointer;
		dwBytes: SizeT
	): Pointer;
	
PROCEDURE ImportProc(
	VAR proc: ARRAY OF SYSTEM.BYTE;
	library, name: ARRAY OF CHAR
);
	VAR lib: Handle; procAdr: Pointer; i: INTEGER;
		ansiStr: ARRAY 256 OF BYTE;
BEGIN
	SYSTEM.LoadLibraryW(lib, library);
	IF lib # 0 THEN i := -1;
		REPEAT i := i + 1; ansiStr[i] := ORD(name[i]) UNTIL name[i] = 0X;
		SYSTEM.GetProcAddress(procAdr, lib, SYSTEM.ADR(ansiStr))
	ELSE procAdr := 0
	END;
	SYSTEM.PUT(SYSTEM.ADR(proc), procAdr)
END ImportProc;

PROCEDURE AsDword(x: INTEGER): INTEGER;
BEGIN
	RETURN x MOD 100000000H
END AsDword;

PROCEDURE AsWord(x: INTEGER): INTEGER;
	RETURN x MOD 10000H
END AsWord;

PROCEDURE AsInt(x: INTEGER): INTEGER;
	RETURN ASR(LSL(x, 32), 32)
END AsInt;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* File functions *)
	
PROCEDURE Existed*(filename: ARRAY OF CHAR): BOOLEAN;
	CONST INVALID_FILE_ATTRIBUTES = {0..31};
	VAR dwRes: Dword;
BEGIN
	dwRes := GetFileAttributesW(SYSTEM.ADR(filename));
	RETURN dwRes # ORD(INVALID_FILE_ATTRIBUTES)
END Existed;
	
PROCEDURE Open*(VAR file: File; filename: ARRAY OF CHAR);
	CONST OPEN_EXISTING = 3;
BEGIN
	file.handle := CreateFileW(
		SYSTEM.ADR(filename), ORD(GENERIC_READ + GENERIC_WRITE),
		0, 0, OPEN_EXISTING, 0, 0
	)
END Open;
	
PROCEDURE Rewrite*(VAR file: File; filename: ARRAY OF CHAR);
	CONST CREATE_ALWAYS = 2;
BEGIN
	file.handle := CreateFileW(
		SYSTEM.ADR(filename), ORD(GENERIC_READ + GENERIC_WRITE),
		0, 0, CREATE_ALWAYS, 0, 0
	)
END Rewrite;

PROCEDURE Close*(VAR file: File);
	VAR bRes: Bool;
BEGIN
	bRes := CloseHandle(file.handle); file.handle := 0
END Close;

PROCEDURE Rename*(oldname, newname: ARRAY OF CHAR);
	VAR bRes: Bool;
BEGIN
	bRes := MoveFileW(SYSTEM.ADR(oldname), SYSTEM.ADR(newname))
END Rename;

PROCEDURE Delete*(filename: ARRAY OF CHAR);
	VAR bRes: Bool;
BEGIN
	bRes := DeleteFileW(SYSTEM.ADR(filename))
END Delete;

(* -------------------------------------------------------------------------- *)
(* Read *)

PROCEDURE Read1*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; buf: BYTE; byteRead: Dword;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(buf), 1, SYSTEM.ADR(byteRead), 0);
	IF (bRes = 0) OR (AsDword(byteRead) # 1) THEN n := -1 ELSE n := buf END
END Read1;
	
PROCEDURE Read2*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; buf: Word; byteRead: Dword;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(buf), 2, SYSTEM.ADR(byteRead), 0);
	IF (bRes = 0) OR (AsDword(byteRead) # 2) THEN n := -1
	ELSE n := AsWord(buf)
	END
END Read2;

PROCEDURE ReadStr*(VAR file: File; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Read2(file, n);
		IF n = -1 THEN n := 0 END; str[i] := CHR(n)
	UNTIL n = 0
END ReadStr;
	
PROCEDURE Read4*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; buf, byteRead: Dword;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(buf), 4, SYSTEM.ADR(byteRead), 0);
	IF (bRes = 0) OR (AsDword(byteRead) # 4) THEN n := -1
	ELSE n := AsDword(buf)
	END
END Read4;
	
PROCEDURE Read8*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword; buf: INTEGER;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(buf), 8, SYSTEM.ADR(byteRead), 0);
	n := buf
END Read8;

PROCEDURE ReadBytes*(
	VAR file: File; VAR buf: ARRAY OF SYSTEM.BYTE; VAR byteRead: INTEGER
);
	VAR bRes: Bool; dwByteRead: Dword;
BEGIN
	bRes := ReadFile(
		file.handle, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(dwByteRead), 0
	);
	byteRead := AsDword(dwByteRead)
END ReadBytes;

(* -------------------------------------------------------------------------- *)
(* Write *)
	
PROCEDURE Write1*(VAR file: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, SYSTEM.ADR(n), 1, SYSTEM.ADR(byteWritten), 0
	)
END Write1;
	
PROCEDURE Write2*(VAR file: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, SYSTEM.ADR(n), 2, SYSTEM.ADR(byteWritten), 0
	)
END Write2;

PROCEDURE WriteStr*(VAR file: File; str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write2(file, ORD(str[i])) UNTIL str[i] = 0X
END WriteStr;

PROCEDURE WriteAnsiStr*(VAR file: File; str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write1(file, ORD(str[i])) UNTIL str[i] = 0X
END WriteAnsiStr;
	
PROCEDURE Write4*(VAR file: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, SYSTEM.ADR(n), 4, SYSTEM.ADR(byteWritten), 0
	)
END Write4;
	
PROCEDURE Write8*(VAR file: File; n: INTEGER);
	VAR bRes: Bool; byteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, SYSTEM.ADR(n), 8, SYSTEM.ADR(byteWritten), 0
	)
END Write8;

PROCEDURE WriteBytes*(
	VAR file: File; buf: ARRAY OF SYSTEM.BYTE; VAR byteWritten: INTEGER
);
	VAR bRes: Bool; dwByteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, SYSTEM.ADR(buf), LEN(buf), SYSTEM.ADR(dwByteWritten), 0
	);
	byteWritten := AsDword(dwByteWritten)
END WriteBytes;

PROCEDURE WriteBytes2*(
	VAR file: File; pBuf: INTEGER; VAR byteWritten: INTEGER
);
	VAR bRes: Bool; dwByteWritten: Dword;
BEGIN
	bRes := WriteFile(
		file.handle, pBuf, byteWritten, SYSTEM.ADR(dwByteWritten), 0
	);
	byteWritten := AsDword(dwByteWritten)
END WriteBytes2;

(* -------------------------------------------------------------------------- *)

PROCEDURE FilePos*(VAR file: File): INTEGER;
	CONST FILE_CURRENT = 1;
	VAR bRes: Bool; byteToMove, newPointer: LargeInteger;
BEGIN byteToMove := 0;
	bRes := SetFilePointerEx(
		file.handle, byteToMove, SYSTEM.ADR(newPointer), FILE_CURRENT
	);
	RETURN newPointer
END FilePos;

PROCEDURE Seek* (VAR file: File; pos: INTEGER);
	CONST FILE_BEGIN = 0;
	VAR bRes: Bool; byteToMove, newPointer: LargeInteger;
BEGIN byteToMove := pos;
	bRes := SetFilePointerEx(
		file.handle, byteToMove, SYSTEM.ADR(newPointer), FILE_BEGIN
	);
END Seek;

PROCEDURE SeekRel*(VAR file: File; offset: INTEGER);
	CONST FILE_CURRENT = 1;
	VAR bRes: Bool; byteToMove, newPointer: LargeInteger;
BEGIN byteToMove := offset;
	bRes := SetFilePointerEx(
		file.handle, byteToMove, SYSTEM.ADR(newPointer), FILE_CURRENT
	);
END SeekRel;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Console *)

PROCEDURE Console_Write*(ch: CHAR);
	CONST CP_UTF8 = 65001; STD_OUTPUT_HANDLE = -11;
	VAR buf: ARRAY 8 OF BYTE;
		size: Int; bRes: Bool; dwByteWritten: Dword;
BEGIN
	size := WideCharToMultiByte(
		CP_UTF8, 0, SYSTEM.ADR(ch), 1, SYSTEM.ADR(buf), 0, 0, 0
	);
	IF size <= LEN(buf) THEN
		size := WideCharToMultiByte(
			CP_UTF8, 0, SYSTEM.ADR(ch), 1, SYSTEM.ADR(buf), size, 0, 0
		);
		bRes := WriteFile(
			GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(buf), size,
			SYSTEM.ADR(dwByteWritten), 0
		)
	END
END Console_Write;

PROCEDURE Console_WriteStr*(str: ARRAY OF CHAR);
	CONST CP_UTF8 = 65001; STD_OUTPUT_HANDLE = -11;
	VAR buf: ARRAY 256 OF BYTE; strlen: INTEGER;
		size: Int; bRes: Bool; dwByteWritten: Dword;
BEGIN
	strlen := 0; WHILE str[strlen] # 0X DO INC(strlen) END;
	size := WideCharToMultiByte(
		CP_UTF8, 0, SYSTEM.ADR(str), strlen, SYSTEM.ADR(buf), 0, 0, 0
	);
	IF size <= LEN(buf) THEN
		size := WideCharToMultiByte(
			CP_UTF8, 0, SYSTEM.ADR(str), strlen, SYSTEM.ADR(buf), size, 0, 0
		);
		bRes := WriteFile(
			GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(buf), size,
			SYSTEM.ADR(dwByteWritten), 0
		)
	END
END Console_WriteStr;

PROCEDURE Console_WriteLn*;
	CONST STD_OUTPUT_HANDLE = -11;
	VAR buf: ARRAY 2 OF BYTE; bRes: Bool;
		dwByteWritten: Dword;
BEGIN buf[0] := 13; buf[1] := 10;
	bRes := WriteFile(
		GetStdHandle(STD_OUTPUT_HANDLE), SYSTEM.ADR(buf), 2,
		SYSTEM.ADR(dwByteWritten), 0
	)
END Console_WriteLn;

PROCEDURE Console_WriteInt*(x: INTEGER);
	CONST MinInt = -9223372036854775807-1;
	VAR neg: BOOLEAN; s, str: ARRAY 32 OF CHAR; i, j: INTEGER;
BEGIN
	IF x # MinInt THEN i := 0; j := 0;
		IF x < 0 THEN neg := TRUE; x := -x ELSE neg := FALSE END;
		REPEAT s[i] := CHR(x MOD 10 + ORD('0')); INC(i); x := x DIV 10
		UNTIL x = 0;		
		IF ~neg THEN
			WHILE (j < i) & (j < LEN(str)) DO
				str[j] := s[i-1-j]; INC(j)
			END;
			str[j] := 0X
		ELSE str[0] := '-';
			WHILE (j < i) & (j < LEN(str)-1) DO
				str[j+1] := s[i-1-j]; INC(j)
			END;
			str[j+1] := 0X
		END
	ELSE str := '-9223372036854775808'
	END;
	Console_WriteStr(str)
END Console_WriteInt;

PROCEDURE Console_WriteHex*(x: INTEGER);
	VAR i, y: INTEGER;
BEGIN
	IF x # 0 THEN i := 15;
		WHILE ASR(x, i*4) MOD 16 = 0 DO DEC(i) END;
		WHILE i >= 0 DO
			y := ASR(x, i*4) MOD 16;
			IF y < 10 THEN Console_Write(CHR(y+ORD('0')))
			ELSE Console_Write(CHR(ORD('A')-10+y))
			END; DEC(i)
		END
	ELSE Console_Write('0')
	END
END Console_WriteHex;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetTickCount*(): INTEGER;
	RETURN GetTickCount_()
END GetTickCount;

PROCEDURE GetArg*(VAR out: ARRAY OF CHAR; VAR paramLen: INTEGER; n: INTEGER);
	CONST chSize = 2;
	VAR i, k: INTEGER; buf: Pointer; ch: CHAR;
BEGIN buf := GetCommandLineW(); i := 0;
	WHILE n > 0 DO
		SYSTEM.GET(buf, ch);
		WHILE (ch # ' ') & (ch # 0X) DO
			buf := buf + chSize; SYSTEM.GET(buf, ch)
		END;
		IF ch = 0X THEN n := 0
		ELSIF ch = ' ' THEN DEC(n);
			WHILE ch = ' ' DO buf := buf + chSize; SYSTEM.GET(buf, ch) END
		END
	END;
	k := 0; paramLen := 0;
	WHILE (ch # ' ') & (ch # 0X) DO
		IF k < LEN(out) THEN out[k] := ch END;
		INC(k); INC(paramLen); buf := buf + chSize; SYSTEM.GET(buf, ch)
	END;
	IF k < LEN(out) THEN out[k] := 0X END
END GetArg;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* MemFile *)

PROCEDURE NewMemFile*(VAR f: MemFile);
	VAR hHeap: Handle;
BEGIN NEW(f);
	hHeap := GetProcessHeap(); f.ptr := HeapAlloc(hHeap, 0, memFileBlk);
	f.maxlen := memFileBlk; f.len := 0
END NewMemFile;

PROCEDURE DeleteMemFile*(f: MemFile);
	VAR hHeap: Handle; bRes: Bool;
BEGIN hHeap := GetProcessHeap(); bRes := HeapFree(hHeap, 0, f.ptr);
	f.ptr := 0; f.maxlen := 0; f.len := 0
END DeleteMemFile;

PROCEDURE ExtendMemFile(f: MemFile; amount: INTEGER);
	VAR hHeap: Handle;
BEGIN amount := amount + (-amount) MOD memFileBlk;
	hHeap := GetProcessHeap(); INC(f.maxlen, amount);
	f.ptr := HeapReAlloc(hHeap, 0, f.ptr, f.maxlen)
END ExtendMemFile;

PROCEDURE MergeMemFile*(f1, f2: MemFile);
	VAR newLen: INTEGER;
BEGIN
	IF f1.maxlen < f1.len + f2.len THEN
		newLen := f1.len + f2.len; newLen := newLen + (-newLen) MOD memFileBlk;
		ExtendMemFile(f1, newLen - f1.maxlen)
	END;
	SYSTEM.COPY(f2.ptr, f1.ptr + f1.len, f2.len);
	INC(f1.len, f2.len); DeleteMemFile(f2)
END MergeMemFile;

PROCEDURE MemFileLength*(f: MemFile): INTEGER;
BEGIN
	RETURN f.len
END MemFileLength;

PROCEDURE SetMemFile*(VAR r: MemFileRider; f: MemFile; pos: INTEGER);
BEGIN r.f := f; r.eof := FALSE;
	IF pos >= 0 THEN
		IF pos <= f.len THEN r.pos := pos ELSE r.pos := f.len END
	ELSE r.pos := 0
	END
END SetMemFile;

PROCEDURE WriteMemFile*(VAR r: MemFileRider; x: BYTE);
BEGIN
	IF r.pos <= r.f.len THEN
		IF r.pos+1 > r.f.maxlen THEN ExtendMemFile(r.f, memFileBlk) END;
		SYSTEM.PUT(r.f.ptr + r.pos, x); INC(r.pos);
		IF r.pos > r.f.len THEN INC(r.f.len) END
	ELSE r.eof := TRUE
	END
END WriteMemFile;

PROCEDURE ReadMemFile*(VAR r: MemFileRider; VAR x: BYTE);
BEGIN
	IF r.pos < r.f.len THEN SYSTEM.GET(r.f.ptr + r.pos, x); INC(r.pos)
	ELSE r.eof := TRUE
	END
END ReadMemFile;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE Init;
BEGIN
	ImportProc(GetFileAttributesW, Kernel32, 'GetFileAttributesW');
	ImportProc(CreateFileW, Kernel32, 'CreateFileW');
	ImportProc(CloseHandle, Kernel32, 'CloseHandle');
	ImportProc(MoveFileW, Kernel32, 'MoveFileW');
	ImportProc(DeleteFileW, Kernel32, 'DeleteFileW');
	ImportProc(ReadFile, Kernel32, 'ReadFile');
	ImportProc(WriteFile, Kernel32, 'WriteFile');
	ImportProc(SetFilePointerEx, Kernel32, 'SetFilePointerEx');
	ImportProc(GetTickCount_, Kernel32, 'GetTickCount');
	ImportProc(GetCommandLineW, Kernel32, 'GetCommandLineW');
	ImportProc(WideCharToMultiByte, Kernel32, 'WideCharToMultiByte');
	ImportProc(GetStdHandle, Kernel32, 'GetStdHandle');
	ImportProc(GetProcessHeap, Kernel32, 'GetProcessHeap');
	ImportProc(HeapAlloc, Kernel32, 'HeapAlloc');
	ImportProc(HeapFree, Kernel32, 'HeapFree');
	ImportProc(HeapReAlloc, Kernel32, 'HeapReAlloc')
END Init;

BEGIN Init
END BaseSys.