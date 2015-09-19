MODULE Editor;
(*$MAIN*)

IMPORT
	SYSTEM, Kernel32, User32, Comdlg32;
	
CONST
	FileMenuId = 0;
	EditMenuId = 1;
	FontMenuId = 2;
	
TYPE
	BOOL = CARD32;

VAR
	hInst, hwnd: INTEGER;
	
PROCEDURE ZeroMemory (VAR buf: ARRAY OF SYSTEM.BYTE);
	VAR i: INTEGER;
BEGIN i := 0; WHILE i < LEN(buf) DO buf[i] := 0; INC (i) END
END ZeroMemory;
	
PROCEDURE StdWindowProc (hwnd: INTEGER; uMsg: CARD32; wParam, lParam: INTEGER): INTEGER;
	VAR bRes: BOOL; result, menuId: INTEGER; callDef: BOOLEAN;
		cf: Comdlg32.CHOOSEFONTW;
BEGIN result := 0; callDef := FALSE;
	IF uMsg = User32.WM_CLOSE THEN
		bRes := User32.DestroyWindow (hwnd)
	ELSIF uMsg = User32.WM_DESTROY THEN
		User32.PostQuitMessage (0)
	ELSIF uMsg = User32.WM_COMMAND THEN
		menuId := wParam MOD 10000H;
		IF menuId = FontMenuId THEN
			ZeroMemory (cf);
			cf.lStructSize := SYSTEM.SIZE(Comdlg32.CHOOSEFONTW);
			bRes := Comdlg32.ChooseFontW (cf)
		ELSE callDef := TRUE
		END
	ELSE callDef := TRUE
	END;
	IF callDef THEN
		result := User32.DefWindowProcW (hwnd, uMsg, wParam, lParam)
	END;
	RETURN result
END StdWindowProc;

PROCEDURE CreateRichEdit;
	CONST MSFTEDIT_CLASS = 'RICHEDIT50W';
	VAR hEdit: INTEGER; rect: User32.RECT; bRes: BOOL;
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
		res16: CARD16; bRes: BOOL; hDll, hMenu: INTEGER;
BEGIN
	SYSTEM.LoadLibraryW (hDll, 'Msftedit.dll'); ASSERT (hDll # 0);
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