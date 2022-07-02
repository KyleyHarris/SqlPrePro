_LoopCursor
MACRO _LoopCursor @cursor, @params, @code
OPEN @cursor
FETCH NEXT FROM @cursor INTO @params
WHILE @@FETCH_STATUS = 0
BEGIN
  @code
  FETCH NEXT FROM @cursor INTO @params  
END
      
CLOSE @cursor
