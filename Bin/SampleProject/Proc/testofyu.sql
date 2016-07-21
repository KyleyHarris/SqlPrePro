testofyu
create procedure testofyu as
begin
$PROC_BEGIN
 
DECLARE @ID $GUID, @Email VarChar(120)

_ExecCursor( MyCursor,
  =( Select Id, Email From Account where name like 'K%'),
  =( @ID, @Email ),
  =( insert into test (key, value) values (@ID, @Email) ) 
)

$PROC_END
end
