MODULE TextFrames;

IMPORT
	SYSTEM, Kernel32, User32, Gdi32, Texts;
	
CONST
	className = 'TextFrame';
	
TYPE
	TextFrame* = POINTER TO EXTENSIBLE RECORD
		hwnd: INTEGER;
		txt: Texts.Text;
		org: INT32
	END;
	
	NewTextFrameParam* = RECORD
		hwndParent*: INTEGER;
		txt*: Texts.Text;
		x*, y*, w*, h*: INTEGER
	END;

VAR
	hInst: INTEGER;
	
PROCEDURE NewTextFrame (VAR tf: TextFrame; par: NewTextFrameParam);
	VAR hwnd, iRes: INTEGER;
BEGIN
	hwnd := User32.CreateWindowExW(
		0, className, NIL, ORD(User32.WS_CHILD), par.x, par.y, par.w, par.h,
		par.hwndParent, 0, hInst, 0
	);
	NEW (tf); tf.hwnd := hwnd; tf.txt := par.txt; tf.org := 0;
	iRes := User32.SetWindowLongPtrW (hwnd, 0, SYSTEM.VAL(INTEGER, tf))
END NewTextFrame;

PROCEDURE RenderTextFrame (tf: TextFrame);
BEGIN
END RenderTextFrame;

PROCEDURE WndProc (hWnd: INTEGER; uMsg: CARD32; wParam, lParam: INTEGER): INTEGER;
	VAR result: INTEGER; tf: TextFrame;
BEGIN result := 0;
	IF uMsg = User32.WM_PAINT THEN
		tf := SYSTEM.VAL(TextFrame, User32.GetWindowLongPtrW(hWnd, 0));
		ASSERT (tf # NIL); RenderTextFrame (tf)
	ELSE result := User32.DefWindowProcW (hWnd, uMsg, wParam, lParam)
	END;
	RETURN result
END WndProc;

PROCEDURE InitModule;
	VAR wordRes: CARD16; wcls: User32.WNDCLASSW;
BEGIN
	hInst := Kernel32.GetModuleHandleW(NIL);
	wcls.style := ORD(User32.CS_GLOBALCLASS);
	wcls.lpfnWndProc := WndProc;
	wcls.cbClsExtra := 0;
	wcls.cbWndExtra := 8;
	wcls.hInstance := hInst;
	wcls.hIcon := 0;
	wcls.hCursor := 0;
	wcls.hbrBackground := 0;
	wcls.lpszMenuName := 0;
	wcls.lpszClassName := SYSTEM.ADR(className);
	wordRes := User32.RegisterClassW (wcls)
END InitModule;

BEGIN
	InitModule
END TextFrames.