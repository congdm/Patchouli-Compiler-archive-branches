MODULE Texts;

IMPORT
	SYSTEM;
	
CONST
	BufferSize = 100000H;

TYPE
	Buffer = POINTER TO RECORD
		data: ARRAY BufferSize OF CHAR
	END;

	Piece = POINTER TO RECORD
		off, len: INT32;
		next, prev: Piece
	END;

	Text* = EXTENSIBLE RECORD
		buffer: Buffer;
		bufOff, org: INT32;
		trailer, pce: Piece
	END;
	
PROCEDURE New* (VAR txt: Text);
	VAR pc, trailer: Piece;
BEGIN
	NEW (txt.buffer); txt.bufOff := 0; NEW (pc); pc.off := 0; pc.len := 0;
	NEW (trailer); pc.next := trailer; pc.prev := trailer; trailer.next := pc;
	trailer.prev := pc; txt.trailer := trailer;
	txt.pce := pc; txt.org := 0 (* translation cache *)
END New;

PROCEDURE CleanBuffer (VAR txt: Text);
	VAR pc, prev, trailer: Piece; oldBuf, newBuf: Buffer; i, byteCnt: INTEGER;
BEGIN trailer := txt.trailer;
	pc := trailer.next; oldBuf := txt.buffer; NEW (newBuf); i := 0;
	WHILE pc # trailer DO
		byteCnt := pc.len * SYSTEM.SIZE(CHAR);
		SYSTEM.COPY(
			SYSTEM.ADR(oldBuf.data[pc.off]),
			SYSTEM.ADR(newBuf.data[i]),
			byteCnt
		);
		i := i + pc.len; prev := pc; pc := pc.next; DISPOSE (prev)
	END;
	NEW (pc); pc.off := 0; pc.len := i; pc.prev := trailer; pc.next := trailer;
	trailer.next := pc; trailer.prev := pc; txt.bufOff := i;
	txt.pce := pc; txt.org := 0; txt.buffer := newBuf; DISPOSE (oldBuf)
END CleanBuffer;

PROCEDURE RemainingChars (txt: Text): INTEGER;
BEGIN
	RETURN BufferSize - txt.bufOff
END RemainingChars;

PROCEDURE FindPiece (VAR txt: Text; pos: INTEGER; VAR org: INTEGER; VAR result: Piece);
	VAR p: Piece; porg, d: INTEGER;
BEGIN porg := txt.org; p := txt.pce; d := 0;
	IF pos >= porg THEN
		WHILE pos >= porg + p.len DO
			porg := porg + p.len; p := p.next; INC (d)
		END
	ELSE REPEAT p := p.prev; porg := porg - p.len; INC (d) UNTIL pos >= porg
	END;
	IF d >= 50 THEN txt.pce := p; txt.org := porg END;
	org := porg; result := p
END FindPiece;

PROCEDURE SplitPiece (p: Piece; off: INTEGER; VAR pr: Piece);
	VAR q: Piece;
BEGIN
	IF off > 0 THEN NEW (q);
		q.len := p.len - off; q.off := p.off + off; p.len := off;
		q.next := p.next; p.next := q; q.prev := p; q.next.prev := q;
		pr := q
	ELSE pr := p
	END
END SplitPiece;
	
END Texts.