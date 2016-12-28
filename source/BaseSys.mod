MODULE BaseSys;
(*$NEW Rtl.New*)

IMPORT
	SYSTEM, Rtl;
	
CONST
	memFileBlk = 128;
	
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
		
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* MemFile *)

PROCEDURE NewMemFile*(VAR f: MemFile);
	VAR hHeap: Handle;
BEGIN NEW(f); Rtl.Alloc(f.ptr, memFileBlk);
	f.maxlen := memFileBlk; f.len := 0
END NewMemFile;

PROCEDURE DeleteMemFile*(f: MemFile);
	VAR hHeap: Handle; bRes: Bool;
BEGIN 
	Rtl.Free(f.ptr); f.ptr := 0; f.maxlen := 0; f.len := 0
END DeleteMemFile;

PROCEDURE ExtendMemFile(f: MemFile; amount: INTEGER);
	VAR hHeap: Handle;
BEGIN amount := amount + (-amount) MOD memFileBlk;
	INC(f.maxlen, amount); Rtl.ReAlloc(f.ptr, f.maxlen)
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
		IF r.pos > r.f.len THEN r.f.len := r.pos END
	ELSE r.eof := TRUE
	END
END WriteMemFile;

PROCEDURE WriteMemFile8*(VAR r: MemFileRider; x: INTEGER);
BEGIN
	IF r.pos <= r.f.len THEN
		IF r.pos+8 > r.f.maxlen THEN ExtendMemFile(r.f, memFileBlk) END;
		SYSTEM.PUT(r.f.ptr + r.pos, x); INC(r.pos, 8);
		IF r.pos > r.f.len THEN r.f.len := r.pos END
	ELSE r.eof := TRUE
	END
END WriteMemFile8;

PROCEDURE ReadMemFile*(VAR r: MemFileRider; VAR x: BYTE);
BEGIN
	IF r.pos < r.f.len THEN SYSTEM.GET(r.f.ptr + r.pos, x); INC(r.pos)
	ELSE r.eof := TRUE
	END
END ReadMemFile;

PROCEDURE MemFileToDisk*(mf: MemFile; f: Rtl.File);
	VAR byteWritten: INTEGER;
BEGIN byteWritten := mf.len;
	Rtl.WriteBuf(f, mf.ptr, byteWritten) 
END MemFileToDisk;

END BaseSys.