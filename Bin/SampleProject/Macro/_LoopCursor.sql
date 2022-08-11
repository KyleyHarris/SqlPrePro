_LoopCursor
MACRO _LoopCursor @cursor, @params, @code
OPEN @cursor
_FETCH(@cursor) @params
WHILE _FETCH_OK()
BEGIN
  @code
  _FETCH(@cursor) @params  
END
CLOSE @cursor
