MODULE TestTexts;
(*$CONSOLE*)

IMPORT
	Console, Texts;
	
PROCEDURE Main;
	CONST str1 = 'Hi. How are you?';
	VAR txt: Texts.Text; buf: ARRAY 256 OF CHAR;
		rd: Texts.Reader;
BEGIN
	Texts.NewText (txt);
	Texts.InsertText (txt, 0, LEN(str1), str1);
	Texts.NewReader (rd, txt); Texts.Read (rd, buf, LEN(buf)); buf[rd.actualRead] := 0X;
	Console.WriteString (buf); Console.WriteLn;
	Texts.InsertText (txt, 2, 5, ' Cong');
	Texts.NewReader (rd, txt); Texts.Read (rd, buf, LEN(buf)); buf[rd.actualRead] := 0X;
	Console.WriteString (buf);
END Main;
	
BEGIN
	Main
END TestTexts.