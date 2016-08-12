(*
	Project Oberon, Revised Edition 2013

	Book copyright (C)2013 Niklaus Wirth and Juerg Gutknecht;
	software copyright (C)2013 Niklaus Wirth (NW), Juerg Gutknecht (JG), Paul
	Reed (PR/PDR).

	Permission to use, copy, modify, and/or distribute this software and its
	accompanying documentation (the "Software") for any purpose with or
	without fee is hereby granted, provided that the above copyright notice
	and this permission notice appear in all copies.

	THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHORS DISCLAIM ALL WARRANTIES
	WITH REGARD TO THE SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
	MERCHANTABILITY, FITNESS AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
	AUTHORS BE LIABLE FOR ANY CLAIM, SPECIAL, DIRECT, INDIRECT, OR
	CONSEQUENTIAL DAMAGES OR ANY DAMAGES OR LIABILITY WHATSOEVER, WHETHER IN
	AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
	CONNECTION WITH THE DEALINGS IN OR USE OR PERFORMANCE OF THE SOFTWARE.
*)

MODULE Scanner0; (* Modified from ORS module in Project Oberon *)

IMPORT
	SYSTEM, Console, Base := Base0;
  
CONST
	MaxIdLen = Base.MaxIdLen;
    NKW = 37;  (* Number of keywords *)
    maxExp = 38; stringBufSize = 256;
  
    (* Symbols *)
    null* = 0; times* = 1; rdiv* = 2; div* = 3; mod* = 4;
    and* = 5; plus* = 6; minus* = 7; or* = 8; eql* = 9;
    neq* = 10; lss* = 11; leq* = 12; gtr* = 13; geq* = 14;
    in* = 15; is* = 16; arrow* = 17; period* = 18;
    char* = 20; int* = 21; real* = 22; false* = 23; true* = 24;
    nil* = 25; string* = 26; not* = 27; lparen* = 28; lbrak* = 29;
    lbrace* = 30; ident* = 31;
    if* = 32; while* = 34; repeat* = 35; case* = 36; for* = 37;
    comma* = 40; colon* = 41; becomes* = 42; upto* = 43; rparen* = 44;
    rbrak* = 45; rbrace* = 46; then* = 47; of* = 48; do* = 49;
    to* = 50; by* = 51; semicolon* = 52; end* = 53; bar* = 54;
    else* = 55; elsif* = 56; until* = 57;
    array* = 60; record* = 61; union* = 62; pointer* = 63; address* = 64;
	const* = 70; type* = 71; var* = 72; procedure* = 73; begin* = 74; 
	return* = 75; import* = 76; module* = 77;
	extensible* = 80; definition* = 81;

VAR
	ival*, slen*: INTEGER;
    rval*: REAL;
    id*: Base.IdStr;
    str*: Base.String; ansiStr*: BOOLEAN;
    errcnt*: INTEGER;

PROCEDURE Mark*(msg: ARRAY OF CHAR);
END Mark;

PROCEDURE Get*(VAR sym: INTEGER);
END Get;
	
END Scanner0.