create procedure testofyu as
begin
SET NOCOUNT ON
 
DECLARE @ID UniqueIdentifier, @Email VarChar(120)


-- Create a local fast cursor called MyCursor
DECLARE MyCursor CURSOR LOCAL FAST_FORWARD for
  Select Id, Email From Account where name like 'K%'

-- prepare to read  
OPEN MyCursor

-- fetch first row
FETCH NEXT FROM MyCursor INTO @ID, @Email
WHILE @@FETCH_STATUS = 0
BEGIN

  -- BEGIN Execute Row Code 
  insert into test (key, value) values (@ID, @Email)
  -- END Execute Row Code 
  
  FETCH NEXT FROM MyCursor INTO @ID, @Email  
END

-- Make Sure we close the cursor      
CLOSE MyCursor
-- Local cursor deallocate automatically.

SET NOCOUNT OFF
end
