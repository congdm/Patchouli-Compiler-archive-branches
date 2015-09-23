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

	Text* = POINTER TO EXTENSIBLE RECORD
		buffer: Buffer;
		bufOff, org, len: INT32;
		trailer, pce: Piece
	END;
	
	Reader* = POINTER TO EXTENSIBLE RECORD
		txt: Text;
		pos: INTEGER
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

PROCEDURE RemainingChars (txt: Text): INTEGER;
BEGIN
	RETURN BufferSize - txt.bufOff
END RemainingChars;

PROCEDURE FindPiece (txt: Text; pos: INTEGER; VAR org: INTEGER; VAR result: Piece);
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

PROCEDURE NewReader (VAR result: Reader; txt: Text);
	VAR rd: Reader;
BEGIN NEW (rd);
	rd.txt := txt; rd.pos := 0;
	result := rd
END NewReader;

PROCEDURE Read (rd: Reader; VAR out: ARRAY OF CHAR; readAmount: INTEGER; VAR actualRead: INTEGER);
	VAR txt: Text; pc: Piece; pos, off, cnt, porg, i, k: INTEGER;
BEGIN txt := rd.txt; pos := rd.pos;
	IF readAmount > LEN(out) THEN readAmount := LEN(out) END;
	IF readAmount > txt.len - pos THEN readAmount := txt.len - pos END;
	IF readAmount > 0 THEN actualRead := readAmount;
		FindPiece (txt, pos, porg, pc); i := 0;
		WHILE readAmount > 0 DO off := pos - porg; cnt := pc.len - off;
			IF cnt > readAmount THEN cnt := readAmount END;
			SYSTEM.COPY(
				SYSTEM.ADR(txt.buffer.data[pc.off + off]),
				SYSTEM.ADR(out[i]),
				cnt * SYSTEM.SIZE(CHAR)
			);
			i := i + cnt; readAmount := readAmount - cnt; pos := pos + cnt;
			porg := porg + pc.len; pc := pc.next
		END;
		rd.pos := pos;
		txt.pce := pc.prev; txt.org := porg - txt.pce.len;
		IF i < LEN(out) THEN out[i] := 0X END
	ELSE actualRead := 0; out[0] := 0X
	END
END Read;
	
END Texts.