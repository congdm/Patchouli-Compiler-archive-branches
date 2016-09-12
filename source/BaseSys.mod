MODULE BaseSys;

IMPORT
	SYSTEM;
	
TYPE
	File* = RECORD handle: INTEGER END;
	
(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)
(* File functions *)
	
PROCEDURE Existed*(filename: ARRAY OF CHAR): BOOLEAN;
	RETURN TRUE
END Existed;
	
PROCEDURE Open*(VAR file: File; filename: ARRAY OF CHAR);
BEGIN
END Open;
	
PROCEDURE Rewrite*(VAR file: File; filename: ARRAY OF CHAR);
BEGIN
END Rewrite;

PROCEDURE Close*(VAR file: File);
BEGIN
END Close;

PROCEDURE Rename*(oldname, newname: ARRAY OF CHAR);
BEGIN
END Rename;

PROCEDURE Delete*(filename: ARRAY OF CHAR);
BEGIN
END Delete;

(* -------------------------------------------------------------------------- *)
(* Read *)

PROCEDURE Read1*(VAR file: File; VAR n: INTEGER);
END Read1;
	
PROCEDURE Read2*(VAR file: File; VAR n: INTEGER);
END Read2;

PROCEDURE ReadStr*(VAR file: File; VAR str: ARRAY OF CHAR);
	VAR i, n: INTEGER;
BEGIN i := -1; n := 0;
	REPEAT INC(i); Read2(file, n); str[i] := CHR(n) UNTIL n = 0
END ReadStr;
	
PROCEDURE Read4*(VAR file: File; VAR n: INTEGER);
BEGIN
END Read4;
	
PROCEDURE Read8*(VAR file: File; VAR n: INTEGER);
BEGIN
END Read8;

PROCEDURE ReadBytes*(
	VAR file: File; VAR buf: ARRAY OF SYSTEM.BYTE; VAR byteRead: INTEGER
);
BEGIN
END ReadBytes;

(* -------------------------------------------------------------------------- *)
(* Write *)
	
PROCEDURE Write1*(VAR file: File; VAR n: INTEGER);
END Write1;
	
PROCEDURE Write2*(VAR file: File; VAR n: INTEGER);
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
	
PROCEDURE Write4*(VAR file: File; VAR n: INTEGER);
BEGIN
END Write4;
	
PROCEDURE Write8*(VAR file: File; VAR n: INTEGER);
BEGIN
END Write8;

PROCEDURE WriteBytes*(
	VAR file: File; VAR buf: ARRAY OF SYSTEM.BYTE; VAR byteWritten: INTEGER
);
BEGIN
END WriteBytes;

(* -------------------------------------------------------------------------- *)

PROCEDURE FilePos*(VAR file: File): INTEGER;
	RETURN 0
END FilePos;

PROCEDURE Seek* (VAR file: File; pos: INTEGER);
BEGIN
END Seek;

PROCEDURE SeekRel*(VAR file: File; offset: INTEGER);
BEGIN
END SeekRel;

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

PROCEDURE GetTickCount*(): INTEGER;
	RETURN 0
END GetTickCount;

PROCEDURE GetArg*(VAR out: ARRAY OF CHAR; VAR paramLen: INTEGER; n: INTEGER);
BEGIN
END GetArg;

END BaseSys.