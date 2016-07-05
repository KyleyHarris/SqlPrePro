_ExecCursor
MACRO _ExecCursor @cursorname, @cursordeclaration, @fields, @codeexec

-- Create a local fast cursor called @cursorname
FastCursor(@cursorname)
  @cursordeclaration

-- prepare to read  
OPEN @cursorname

-- fetch first row
_FETCH(@cursorname) @fields
WHILE _FETCH_OK()
BEGIN

  -- BEGIN Execute Row Code 
  @codeexec
  -- END Execute Row Code 
  
  _FETCH(@cursorname) @fields  
END

-- Make Sure we close the cursor      
CLOSE @cursorname
-- Local cursor deallocate automatically.
