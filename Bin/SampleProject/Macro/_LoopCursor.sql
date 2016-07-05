_LoopCursor
MACRO _LoopCursor @cursor, @params, @code
OPEN Speed
FETCH NEXT FROM Speed INTO @params
WHILE @@FETCH_STATUS = 0
BEGIN
  @code
  FETCH NEXT FROM Speed INTO @params  
END
      
CLOSE Speed
