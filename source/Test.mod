MODULE Test;

	PROCEDURE Main (input : INTEGER);
		VAR i : INTEGER;
	BEGIN
		i := 2;
		input := input + 6;
		i := input
	END Main;
	
BEGIN
	Main (0)
END Test.