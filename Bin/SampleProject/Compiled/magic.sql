




create procedure magic (@SNAME VARCHAR(90)) as 
begin
  SET NOCOUNT ON
  DECLARE @ID UniqueIdentifier            
  DECLARE @NAME VarChar(50)
  DECLARE Speed CURSOR LOCAL FAST_FORWARD for
    Select Id, Name from Account
 
  OPEN Speed
FETCH NEXT FROM Speed INTO @ID, @NAME
WHILE @@FETCH_STATUS = 0
BEGIN
    Insert Into AuditLog (Key, Text) Values (@ID,@Name)
  FETCH NEXT FROM Speed INTO @ID, @NAME  
END
      
CLOSE Speed;
 
  SET NOCOUNT OFF
 
end;                                              
