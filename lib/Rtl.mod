MODULE Rtl;

IMPORT
	SYSTEM;
	
CONST
	Kernel32Path = 'KERNEL32.DLL';
	
TYPE
	Handle = INTEGER;
	Pointer = INTEGER;
	Bool = INTEGER;
	Int = INTEGER;

VAR
	GetProcessHeap: PROCEDURE(): Handle;
	HeapAlloc: PROCEDURE(hHeap, dwFlags, dwBytes: INTEGER): Pointer;
	HeapFree: PROCEDURE(hHeap, dwFlags, lpMem: INTEGER): Bool;
	HeapReAlloc: PROCEDURE(hHeap, dwFlags, lpMem, dwBytes: INTEGER): Pointer;
	ExitProcess*: PROCEDURE(uExitCode: INTEGER);
	
	MessageBoxW: PROCEDURE(hWnd, lpText, lpCaption, uType: INTEGER): Int;
	
	heapBase, heapSize: INTEGER;
	
PROCEDURE ImportExtProc*(
	VAR proc: ARRAY OF SYSTEM.BYTE;
	libPath, procName: ARRAY OF CHAR
);
	VAR hLib, procAdr, i: INTEGER; ansiStr: ARRAY 256 OF BYTE;
BEGIN
	SYSTEM.LoadLibraryW(hLib, libPath);
	IF hLib # 0 THEN i := -1;
		REPEAT INC(i); ansiStr[i] := ORD(procName[i])
		UNTIL (procName[i] = 0X) OR (i = LEN(ansiStr)-1); ansiStr[i] := 0;
		SYSTEM.GetProcAddress(procAdr, hLib, SYSTEM.ADR(ansiStr))
	ELSE procAdr := 0
	END;
	SYSTEM.PUT(SYSTEM.ADR(proc), procAdr)
END ImportExtProc;

PROCEDURE MessageBox*(title, msg: ARRAY OF CHAR);
	VAR iRes: INTEGER;
BEGIN iRes := MessageBoxW(0, SYSTEM.ADR(msg), SYSTEM.ADR(title), 0)
END MessageBox;

PROCEDURE Halt*(msg: ARRAY OF CHAR);
BEGIN MessageBox('Halt', msg); ExitProcess(0)
END Halt;

PROCEDURE ExtendHeap;
	VAR p, mark, size: INTEGER;
BEGIN INC(heapSize, heapSize); p := heapBase;
	heapBase := HeapReAlloc(GetProcessHeap(), 16, heapBase, heapSize);
	IF heapBase = 0 THEN Halt('Out of memory') END;
	WHILE p < heapBase + heapSize DIV 2 DO
		SYSTEM.GET(p, mark); SYSTEM.GET(p+8, size);
		IF mark = 0 THEN SYSTEM.GET(size, size); INC(size, 16);
		ELSIF (mark = -1) OR (mark = -2) THEN (* ok *)
		ELSE Halt('Heap corruption')
		END;
		INC(p, size)
	END;
	IF p # heapBase + heapSize DIV 2 THEN Halt('Heap corruption') END;
	IF mark # -1 THEN SYSTEM.PUT(p, -1); SYSTEM.PUT(p+8, heapSize DIV 2)
	ELSE DEC(p, size); SYSTEM.PUT(p+8, size + heapSize DIV 2)
	END
END ExtendHeap;

PROCEDURE FindFree(need: INTEGER): INTEGER;
	VAR p, mark, size: INTEGER; found: BOOLEAN;
BEGIN p := heapBase; found := FALSE;
	WHILE ~found & (p < heapBase+heapSize) DO
		SYSTEM.GET(p, mark); SYSTEM.GET(p+8, size);
		IF mark = -1 THEN
			IF size >= need THEN found := TRUE ELSE INC(p, size) END
		ELSIF mark = 0 THEN SYSTEM.GET(size, size); INC(p, size+16)
		ELSIF mark = -2 THEN INC(p, size)
		ELSE Halt('Heap corruption')
		END
	END;
	IF ~found THEN p := -1 END;
	RETURN p
END FindFree;
	
PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR p, need, mark, bRes: INTEGER;
BEGIN 
	SYSTEM.GET(tdAdr, need); need := (need + 31) DIV 16 * 16;
	REPEAT p := FindFree(need);
		IF (p = -1) OR (p+need = heapBase+heapSize) THEN ExtendHeap END
	UNTIL p # -1;
	
END New;

PROCEDURE Alloc*(VAR ptr: INTEGER; size: INTEGER);
BEGIN
END Alloc;

BEGIN
	ImportExtProc(GetProcessHeap, Kernel32Path, 'GetProcessHeap');
	ImportExtProc(HeapAlloc, Kernel32Path, 'HeapAlloc');
	ImportExtProc(HeapFree, Kernel32Path, 'HeapFree');
	ImportExtProc(HeapReAlloc, Kernel32Path, 'HeapReAlloc');
	
	heapSize := 80000H;
	heapBase := HeapAlloc(GetProcessHeap(), 0, heapSize);
	IF heapBase = 0 THEN Halt('Cannot init memory manager') END;
	
	SYSTEM.PUT(heapBase, -1); SYSTEM.PUT(heapBase+8, heapSize)
END Rtl.