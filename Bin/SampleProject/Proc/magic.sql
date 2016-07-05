magic
LOCALMACRO _speedparams
@ID, @NAME
ENDMACRO

LOCALMACRO _speedcode
  Insert Into AuditLog (Key, Text) Values (@ID,@Name)
ENDMACRO


create procedure magic (@SNAME VARCHAR(90)) as 
begin
  #PROC_BEGIN#
  DECLARE @ID #GUID#            
  DECLARE @NAME VarChar(50)
  FastCursor(Speed)
    Select Id, Name from Account
 
  _LoopCursor(
    Speed, 
    _SpeedParams(), 
    _SpeedCode()
  );
 
  #PROC_END#
 
end;                                              
