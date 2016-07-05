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
    -- Do SomeThing
    FETCH NEXT FROM Speed INTO @ID, @NAME  
  END
        
  CLOSE Speed
  SET NOCOUNT ON
 
end;                                              
