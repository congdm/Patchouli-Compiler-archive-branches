MODULE Editor;
(*$MAIN*)

IMPORT
	SYSTEM, Kernel32, User32, Gdi32, Comdlg32, Msftedit, Console;
	
CONST
	FileMenuId = 0;
	EditMenuId = 1;
	FontMenuId = 2;
	
TYPE
	BOOL = CARD32;

VAR
	hInst, hwnd, hEdit: INTEGER;
	
	
PROCEDURE ZeroMemory (VAR buf: ARRAY OF SYSTEM.BYTE);
	VAR i: INTEGER;
BEGIN i := 0; WHILE i < LEN(buf) DO buf[i] := 0; INC (i) END
END ZeroMemory;

PROCEDURE Copy (VAR x, y: ARRAY OF SYSTEM.BYTE);
	VAR i: INTEGER;
BEGIN
	ASSERT (LEN(x) = LEN(y));
	FOR i := 0 TO LEN(x) - 1 DO y[i] := x[i] END
END Copy;

PROCEDURE ShowFontDialog;
	VAR cf: Comdlg32.CHOOSEFONTW; chfmt: Msftedit.CHARFORMATW;
		lFont: Gdi32.LOGFONTW; msgResult: INTEGER; bRes: BOOL;
		errCode: CARD32;
BEGIN
	chfmt.cbSize := SYSTEM.SIZE(Msftedit.CHARFORMATW);
	msgResult := User32.SendMessageW(
		hEdit, Msftedit.EM_GETCHARFORMAT,
		ORD(Msftedit.SCF_DEFAULT), SYSTEM.ADR(chfmt)
	);
	ASSERT (msgResult = chfmt.dwMask);
	
	ZeroMemory (lFont);
	Copy (chfmt.szFaceName, lFont.lfFaceName);
	lFont.lfPitchAndFamily := chfmt.bPitchAndFamily;
	lFont.lfHeight := -FLOOR(FLT(chfmt.yHeight) / 15.0);
	lFont.lfCharSet := chfmt.bCharSet;
	
	ZeroMemory (cf);
	cf.lStructSize := SYSTEM.SIZE(Comdlg32.CHOOSEFONTW);
	cf.lpLogFont := SYSTEM.ADR(lFont);
	cf.Flags := ORD(Comdlg32.CF_INITTOLOGFONTSTRUCT);
	bRes := Comdlg32.ChooseFontW (cf);
	
	Copy (lFont.lfFaceName, chfmt.szFaceName);
	chfmt.bPitchAndFamily := lFont.lfPitchAndFamily;
	chfmt.yHeight := -lFont.lfHeight * 15;
	chfmt.bCharSet := lFont.lfCharSet;
	
	msgResult := User32.SendMessageW(
		hEdit, Msftedit.EM_SETCHARFORMAT,
		ORD(Msftedit.SCF_DEFAULT), SYSTEM.ADR(chfmt)
	);
	ASSERT (msgResult # 0);
END ShowFontDialog;
	
PROCEDURE StdWindowProc (hwnd: INTEGER; uMsg: CARD32; wParam, lParam: INTEGER): INTEGER;
	VAR bRes: BOOL; result, menuId: INTEGER;
		
BEGIN result := 0;
	IF uMsg = User32.WM_CLOSE THEN bRes := User32.DestroyWindow (hwnd)
	ELSIF uMsg = User32.WM_DESTROY THEN User32.PostQuitMessage (0)
	ELSIF uMsg = User32.WM_COMMAND THEN
		menuId := wParam MOD 10000H;
		IF menuId = FontMenuId THEN ShowFontDialog END
	ELSE result := User32.DefWindowProcW (hwnd, uMsg, wParam, lParam)
	END;
	RETURN result
END StdWindowProc;

PROCEDURE CreateRichEdit;
	CONST MSFTEDIT_CLASS = 'RICHEDIT50W';
	VAR rect: User32.RECT; bRes: BOOL;
BEGIN
	bRes := User32.GetClientRect (hwnd, rect);
	hEdit := User32.CreateWindowExW(
		0, MSFTEDIT_CLASS, 'Type here',
		ORD(User32.ES_MULTILINE + User32.WS_VISIBLE + User32.WS_CHILD
			+ User32.WS_BORDER + User32.WS_TABSTOP
		),
		0, 0, 790, 555,
		hwnd, 0, hInst, 0
	);
END CreateRichEdit;

PROCEDURE CreateMenu(): INTEGER;
	VAR hMenu: INTEGER;
		
	PROCEDURE CreateItem (hMenu: User32.HANDLE; text: ARRAY OF CHAR; id, pos: INTEGER);
		VAR mii: User32.MENUITEMINFO; bRes: BOOL;
	BEGIN
		mii.cbSize := SYSTEM.SIZE(User32.MENUITEMINFO);
		mii.fMask := ORD(User32.MIIM_STRING + User32.MIIM_ID);
		mii.fType := 0;
		mii.fState := 0;
		mii.wID := id;
		mii.hSubMenu := 0;
		mii.hbmpChecked := 0;
		mii.hbmpUnchecked := 0;
		mii.dwItemData := 0;
		mii.dwTypeData := SYSTEM.ADR(text);
		mii.cch := 0;
		mii.hbmpItem := 0;
		bRes := User32.InsertMenuItemW (hMenu, pos, 1, mii)
	END CreateItem;
		
BEGIN (* CreateMenu *)
	hMenu := User32.CreateMenu();
	CreateItem (hMenu, 'File', FileMenuId, 0);
	CreateItem (hMenu, 'Edit', EditMenuId, 1);
	CreateItem (hMenu, 'Font', FontMenuId, 2);
	RETURN hMenu
END CreateMenu;
	
PROCEDURE Main;
	CONST
		className = 'MainWindow';
	VAR
		wclass: User32.WNDCLASSEXW; msg: User32.MSG;
		res16: CARD16; bRes: BOOL; hMenu: INTEGER;
BEGIN
	hInst := Kernel32.GetModuleHandleW(NIL);
	
	wclass.cbSize := SYSTEM.SIZE(User32.WNDCLASSEXW);
    wclass.style := 0;
    wclass.lpfnWndProc := StdWindowProc;
    wclass.cbClsExtra := 0;
    wclass.cbWndExtra := 0;
    wclass.hInstance := hInst;
    wclass.hIcon := 0;
    wclass.hCursor := 0;
    wclass.hbrBackground := User32.COLOR_WINDOW;
    wclass.lpszMenuName  := 0;
    wclass.lpszClassName := SYSTEM.ADR(className);
	wclass.hIconSm := 0;
	res16 := User32.RegisterClassExW (wclass); ASSERT (res16 # 0);
	
	hMenu := CreateMenu();
	hwnd := User32.CreateWindowExW(
		0, className, 'Editor',
		ORD(User32.WS_OVERLAPPEDWINDOW),
		0, 0, 800, 600,
		0, hMenu, hInst, 0
	);
	bRes := User32.ShowWindow (hwnd, 1);
	CreateRichEdit;
	
	WHILE User32.GetMessageW (msg, 0, 0, 0) > 0 DO
		bRes := User32.TranslateMessage (msg);
		bRes := User32.DispatchMessageW (msg)
	END
END Main;

BEGIN Main
END Editor.