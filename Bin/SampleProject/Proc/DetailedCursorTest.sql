DetailedCursorTest
create procedure DetailedCursorTest as
begin
$PROC_BEGIN
 
DECLARE @ID $GUID, @Email VarChar(120)

_ExecCursor( MyCursor,
  =( Select a.Id, c.Name From Account 
    left join CollationProject c on (c.Id= Account.Id)
     where name like 'K%'),
  =( @ID, @Email ),
  =( 
   
   IF @Email <> 'kyley@harrissoftware.com'
   BEGIN
     insert into test (key, value) values (@ID, @Email)
   END ELSE
   BEGIN
     insert into test (key, value) values (@ID, 'anonymous@Email.com')   
   END 
  
   ) 
)

$PROC_END
end
