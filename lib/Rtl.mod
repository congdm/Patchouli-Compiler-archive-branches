MODULE Rtl; (* multi-threaded application NOT SUPPORTED *)
IMPORT SYSTEM;
	
CONST
	Kernel32Path = 'KERNEL32.DLL';
	heapErrMsg = 'Heap corruption';
	
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
	fList: ARRAY 9 OF INTEGER;
	fList0: INTEGER;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Utility procedures *)
	
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

PROCEDURE Assert*(cond: BOOLEAN; msg: ARRAY OF CHAR);
BEGIN
	IF ~cond THEN Halt(msg) END
END Assert;

PROCEDURE FillByte*(ptr, count: INTEGER; val: BYTE);
BEGIN
	WHILE count > 0 DO SYSTEM.PUT(ptr, val); INC(ptr); DEC(count) END
END FillByte;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* Heap management *)

PROCEDURE ValidMark(mark: INTEGER): BOOLEAN;
	RETURN (mark = 0) OR (mark = -1) OR (mark = -2)
END ValidMark;

PROCEDURE HeapLimit(): INTEGER;
	RETURN heapBase + heapSize
END HeapLimit;

PROCEDURE ExtendHeap;
	VAR p, mark, size, prev, p2: INTEGER;
BEGIN
	heapBase := HeapReAlloc(GetProcessHeap(), 16, heapBase, heapSize*2);
	IF heapBase = 0 THEN Halt('Out of memory') END;
	p := HeapLimit(); SYSTEM.PUT(p+8, heapSize);
	IF fList0 = 0 THEN fList0 := p
	ELSE prev := fList0; SYSTEM.GET(fList0, p2);
		WHILE p2 # 0 DO prev := p2; SYSTEM.GET(p2, p2) END;
		SYSTEM.PUT(prev, p)
	END;
	heapSize := heapSize*2
END ExtendHeap;

PROCEDURE Split(p, need: INTEGER);
	VAR size, i, p2, next: INTEGER;
BEGIN
	SYSTEM.GET(p+8, size); SYSTEM.GET(p, next);
	IF need < size THEN i := (size-need) DIV 64; p2 := p+need;
		SYSTEM.PUT(p+8, need); SYSTEM.PUT(p2+8, size-need);
		IF i < LEN(fList) THEN SYSTEM.PUT(p2, fList[i]); fList[i] := p2
		ELSE SYSTEM.PUT(p2, next); SYSTEM.PUT(p, p2)
		END
	END
END Split;

PROCEDURE Split2(i: INTEGER);
	VAR p, size, need, p2, next, k: INTEGER;
BEGIN p := fList0; need := i*64;
	SYSTEM.GET(p+8, size); SYSTEM.GET(p, next); p2 := p+need;
	SYSTEM.PUT(p+8, need); SYSTEM.PUT(p2+8, size-need);
	k := (size-need) DIV 64;
	IF k >= LEN(fList) THEN fList0 := p2; SYSTEM.PUT(p2, next)
	ELSE fList0 := next;
		IF k # i THEN SYSTEM.PUT(p2, fList[k]); fList[k] := p2
		ELSE SYSTEM.PUT(p2, fList[k]); SYSTEM.PUT(p, p2)
		END
	END;
	fList[i] := p
END Split2;

PROCEDURE Alloc0(need: INTEGER): INTEGER;
	VAR p, prev, next, i, k, size: INTEGER;
BEGIN i := need DIV 64;
	IF i < 3 THEN p := fList[i];
		IF p = 0 THEN 
			IF fList0 = 0 THEN ExtendHeap END; Split2(i); p := fList[i]
		END;
		SYSTEM.GET(p, next); fList[i] := next
	ELSE p := fList0; prev := 0;
		IF p # 0 THEN SYSTEM.GET(p+8, size) END;
		WHILE (p # 0) & (size < need) DO
			prev := p; SYSTEM.GET(p, p);
			IF p # 0 THEN SYSTEM.GET(p+8, size) END
		END;
		IF p # 0 THEN Split(p, need); SYSTEM.GET(p, next);
			IF prev = 0 THEN fList0 := next ELSE SYSTEM.PUT(prev, next) END
		ELSE ExtendHeap; p := Alloc0(need)
		END
	END;
	RETURN p
END Alloc0;

PROCEDURE Free0(p: INTEGER);
	VAR size, i, p2, prev: INTEGER;
BEGIN
	SYSTEM.GET(p+8, size); i := size DIV 64 - 1;
	IF size < 4 THEN SYSTEM.PUT(p, fList[i]); fList[i] := p
	ELSE p2 := fList[4];
		IF (p2 = 0) OR (p2 > p) THEN SYSTEM.PUT(p, p2); fList[4] := p
		ELSE prev := p2; SYSTEM.GET(p2, p2);
			WHILE (p2 # 0) & (p2 < p) DO prev := p2; SYSTEM.GET(p2, p2) END;
			SYSTEM.PUT(p, p2); SYSTEM.PUT(prev, p)
		END
	END
END Free0;

PROCEDURE New*(VAR ptr: INTEGER; tdAdr: INTEGER);
	VAR p, size, i, off: INTEGER;
BEGIN
	SYSTEM.GET(tdAdr, size); size := (size+32+63) DIV 64 * 64;
	p := Alloc0(size); SYSTEM.PUT(p+24, tdAdr); ptr := p+32; INC(p, 32);
	
	i := tdAdr+64; SYSTEM.GET(i, off);
	WHILE off # -1 DO SYSTEM.PUT(p+off, 0); INC(i, 8); SYSTEM.GET(i, off) END
END New;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE InitHeap;
	VAR i: INTEGER;
BEGIN heapSize := 800000H; 
	heapBase := HeapAlloc(GetProcessHeap(), 8, heapSize);
	IF heapBase = 0 THEN Halt('Cannot init heap') END;
	FOR i := 1 TO LEN(fList)-1 DO fList[i] := 0 END; fList0 := heapBase;
	SYSTEM.PUT(heapBase, 0); SYSTEM.PUT(heapBase+8, heapSize)
END InitHeap;

BEGIN
	ImportExtProc(GetProcessHeap, Kernel32Path, 'GetProcessHeap');
	ImportExtProc(HeapAlloc, Kernel32Path, 'HeapAlloc');
	ImportExtProc(HeapFree, Kernel32Path, 'HeapFree');
	ImportExtProc(HeapReAlloc, Kernel32Path, 'HeapReAlloc');
	ImportExtProc(ExitProcess, Kernel32Path, 'ExitProcess');
	ImportExtProc(MessageBoxW, 'USER32.DLL', 'MessageBoxW');
	
	InitHeap
END Rtl.