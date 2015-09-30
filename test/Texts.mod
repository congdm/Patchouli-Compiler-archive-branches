MODULE Texts;

IMPORT
	SYSTEM, Console;
	
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

	Text* = POINTER TO EXTENSIBLE RECORD
		buffer: Buffer;
		bufOff, org, len: INT32;
		trailer, pce: Piece
	END;
	
PROCEDURE NewText* (VAR result: Text);
	VAR pc, trailer: Piece; txt: Text;
BEGIN NEW (txt); NEW (txt.buffer); txt.bufOff := 0; txt.len := 0;
	NEW (pc); pc.off := 0; pc.len := 0;
	NEW (trailer); pc.next := trailer; pc.prev := trailer;
	trailer.next := pc; trailer.prev := pc; txt.trailer := trailer;
	txt.pce := pc; txt.org := 0; (* translation cache *)
	result := txt
END NewText;

PROCEDURE CleanBuffer (txt: Text);
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

PROCEDURE RemainingBuffer (txt: Text): INTEGER;
BEGIN
	RETURN BufferSize - txt.bufOff
END RemainingBuffer;

PROCEDURE FindPiece (txt: Text; pos: INTEGER; VAR org: INTEGER; VAR result: Piece);
	VAR p, trailer: Piece; porg, d: INTEGER;
BEGIN porg := txt.org; p := txt.pce; d := 0;
	IF pos >= porg THEN trailer := txt.trailer;
		WHILE (pos >= porg + p.len) & (p.next # trailer) DO
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

PROCEDURE InsertText* (txt: Text; pos, insertAmount: INTEGER; str: ARRAY OF CHAR);
	VAR pc, pr, pl: Piece; porg, off, free: INTEGER;
BEGIN
	IF insertAmount > LEN(str) THEN insertAmount := LEN(str) END;
	free := BufferSize - txt.len; IF insertAmount > free THEN insertAmount := free END;
	IF RemainingBuffer(txt) < insertAmount THEN CleanBuffer (txt) END;
	IF insertAmount > 0 THEN
		FindPiece (txt, pos, porg, pc); off := pos - porg;
		IF pc.off + off < txt.bufOff THEN
			pl := pc; SplitPiece (pc, off, pr);
			NEW (pc); pc.off := txt.bufOff; pc.len := 0;
			pc.prev := pl; pl.next := pc; pc.next := pr; pr.prev := pc
		ELSIF pc.off + off > txt.bufOff THEN ASSERT(FALSE)
		END;
		SYSTEM.COPY(
			SYSTEM.ADR(str),
			SYSTEM.ADR(txt.buffer.data[txt.bufOff]),
			insertAmount * SYSTEM.SIZE(CHAR)
		);
		pc.len := pc.len + insertAmount;
		txt.len := txt.len + insertAmount;
		txt.bufOff := txt.bufOff + insertAmount
	ELSIF insertAmount < 0 THEN ASSERT(FALSE)
	END
END InsertText;

PROCEDURE ReadText* (txt: Text; VAR result: ARRAY OF CHAR; VAR actualRead: INTEGER);
	VAR trailer, pc: Piece; i, cnt: INTEGER;
BEGIN trailer := txt.trailer; pc := trailer.next; i := 0;
	WHILE pc # trailer DO
		IF i < LEN(result) THEN cnt := LEN(result) - i;
			IF cnt > pc.len THEN cnt := pc.len END;
			SYSTEM.COPY(
				SYSTEM.ADR(txt.buffer.data[pc.off]),
				SYSTEM.ADR(result[i]),
				cnt * SYSTEM.SIZE(CHAR)
			);
			i := i + cnt
		END;
		pc := pc.next
	END;
	actualRead := i
END ReadText;

PROCEDURE DeleteLeft* (txt: Text; pos, amount: INTEGER);
BEGIN
END DeleteLeft;
	
END Texts.