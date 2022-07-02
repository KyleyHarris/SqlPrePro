create procedure DetailedCursorTest as
begin
  SET NOCOUNT ON

  DECLARE @ID UniqueIdentifier, @Email VarChar(120)

  
-- Create a local fast cursor called MyCursor
DECLARE MyCursor CURSOR LOCAL FAST_FORWARD for 

  Select a.Id, a.Email From Account 
       where name like 'K%'

OPEN MyCursor
FETCH NEXT FROM MyCursor INTO @ID, @Email
WHILE @@FETCH_STATUS = 0
BEGIN
  IF @Email <> 'bobo@testsite.com'
   BEGIN
     insert into test (key, value) values (@ID, @Email)
   END ELSE
   BEGIN
     insert into test (key, value) values (@ID, 'anonymous@Email.com')   
   END
  FETCH NEXT FROM MyCursor INTO @ID, @Email  
END
      
CLOSE MyCursor



  SET NOCOUNT OFF
end
