MODULE BaseSys;

IMPORT
	SYSTEM;
	
CONST
	Kernel32 = 'Kernel32.dll';
	GENERIC_READ = {31}; GENERIC_WRITE = {30};
	
TYPE
	Pointer = INTEGER;
	Handle = INTEGER;
	Dword = INTEGER;
	Word = INTEGER;
	Bool = INTEGER;
	LargeInteger = INTEGER;
	
	File* = RECORD handle: Handle END;
	
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
	RETURN x MOD 100000000H
END AsDword;

PROCEDURE AsWord(x: INTEGER): INTEGER;
	RETURN x MOD 10000H
END AsWord;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* File functions *)
	
PROCEDURE Existed*(filename: ARRAY OF CHAR): BOOLEAN;
	CONST INVALID_FILE_ATTRIBUTES = {0..31};
	VAR dwRes: Dword;
BEGIN
	dwRes := GetFileAttributesW(SYSTEM.ADR(filename));
	RETURN dwRes = ORD(INVALID_FILE_ATTRIBUTES)
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
	IF (bRes = 0) OR (AsDword(byteRead) # 1) THEN n := -1
	ELSE n := AsWord(buf)
	END
END Read2;

PROCEDURE ReadStr*(VAR file: File; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Read2(file, n); str[i] := CHR(n) UNTIL n = 0
END ReadStr;
	
PROCEDURE Read4*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; buf, byteRead: Dword;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(buf), 4, SYSTEM.ADR(byteRead), 0);
	IF (bRes = 0) OR (AsDword(byteRead) # 1) THEN n := -1
	ELSE n := AsDword(buf)
	END
END Read4;
	
PROCEDURE Read8*(VAR file: File; VAR n: INTEGER);
	VAR bRes: Bool; byteRead: Dword;
BEGIN
	bRes := ReadFile(file.handle, SYSTEM.ADR(n), 8, SYSTEM.ADR(byteRead), 0)
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

PROCEDURE WriteStr*(VAR file: File; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write2(file, n); str[i] := CHR(n) UNTIL n = 0
END WriteStr;

PROCEDURE WriteAnsiStr*(VAR file: File; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Write1(file, n); str[i] := CHR(n) UNTIL n = 0
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
END BaseSys.