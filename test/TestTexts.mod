MODULE TestTexts;
(*$CONSOLE*)

IMPORT
	Console, Texts;
	
PROCEDURE Main;
	CONST str1 = 'Hi. How are you?';
	VAR txt: Texts.Text; buf: ARRAY 256 OF CHAR;
		actRead: INTEGER;
BEGIN
	Texts.NewText (txt);
	Texts.InsertText (txt, 0, LEN(str1), str1);
	Texts.ReadText (txt, buf, actRead); buf[actRead] := 0X;
	Console.WriteString (buf); Console.WriteLn;
	Texts.InsertText (txt, 2, 5, ' Cong');
	Texts.ReadText (txt, buf, actRead); buf[actRead] := 0X;
	Console.WriteString (buf);
END Main;
	
BEGIN
	Main
END TestTexts.