MODULE MemFiles;
(*$NEW Rtl.New*)
IMPORT SYSTEM, Rtl;

CONST
	blkUnit = 64;
	MEM_RESERVE = 2000H; MEM_COMMIT = 1000H; PAGE_READWRITE = 4;

TYPE
	Block* = POINTER TO RECORD
		pos, size, adr: INTEGER; next: Block
	END;
	File* = POINTER TO RECORD
		first, last: Block; len: INTEGER
	END;
	Rider* = RECORD
		f: File; cur: Block; pos: INTEGER; eof*: BOOLEAN
	END;
	
VAR
	VirtualAlloc: PROCEDURE(
		lpAddress, dwSize, flAllocationType, flProtect: INTEGER
	): INTEGER;
	sAdr, eAdr, cAdr: INTEGER;
	
PROCEDURE AllocPage;
BEGIN
	IF eAdr-sAdr < 80000000H THEN
		eAdr := VirtualAlloc(eAdr, 1000H, MEM_COMMIT, PAGE_READWRITE);
		INC(eAdr, 1000H)
	ELSE ASSERT(FALSE)
	END
END AllocPage;
	
PROCEDURE NewBlock(f: File);
	VAR blk: Block;
BEGIN
	NEW(blk); blk.size := blkUnit; blk.adr := cAdr;
	INC(cAdr, blkUnit); IF cAdr > eAdr THEN AllocPage END;
	IF f.last # NIL THEN
		blk.pos := f.last.pos + f.last.size;
		f.last.next := blk; f.last := blk
	ELSE f.first := blk; f.last := blk; blk.pos := 0
	END;
END NewBlock;
	
PROCEDURE New*(VAR f: File);
BEGIN
	NEW(f); f.len := 0; NewBlock(f)
END New;

PROCEDURE Set*(VAR r: Rider; f: File; pos: INTEGER);
	VAR b: Block;
BEGIN r.f := f; b := f.first;
	IF pos >= f.len THEN pos := f.len; r.eof := TRUE ELSE r.eof := FALSE END;
	WHILE (pos < b.pos) OR (pos > b.pos + b.size) DO b := b.next END;
	IF pos = b.pos + b.size THEN b := 
		r.pos := pos; r.cur := b; r.eof := FALSE
	ELSE r.pos := f.len; r.eof := TRUE; r.cur := f.last
	END
END Set;

PROCEDURE Write*(VAR r: Rider; x: BYTE);
	VAR b: Block; pos: INTEGER;
BEGIN pos := r.pos; b := r.cur;
	SYSTEM.PUT(SYSTEM.ADR(r.cache) + pos DIV 8 * 8, x); INC(pos);
	IF pos MOD 8 = 0 THEN
		SYSTEM.PUT(b.adr + (pos-b.pos) DIV 8 * 8, r.cache);
		IF pos = b.pos + b.size THEN Extend(r.f) END;
		SYSTEM.GET(	
	
END Write;

BEGIN

END MemFiles.