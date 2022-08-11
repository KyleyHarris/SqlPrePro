_ExecCursor
MACRO _ExecCursor @cursorname, @cursordeclaration, @fields, @codeexec
-- Create a local fast cursor called @cursorname
FastCursor(@cursorname) @cursordeclaration
_LoopCursor(@cursorname, =(@fields), =(@codeexec))
