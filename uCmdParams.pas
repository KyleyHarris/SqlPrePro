unit uCmdParams;

interface

function Switch(aSwitchName: string):Boolean;
function SwitchValue(aSwitchName: string):string;

implementation

uses
  System.Classes,
  System.SysUtils;

function IsSwitch(value: string): Boolean;
begin
  Result := (value <> '') and ((value[1] = '/') or (value[1] = '-'));
end;

function SwitchName(value: string): string;
var  i: Integer;
begin
  result := Copy(value, 2,length(value)-1);
  i := pos(':', Result);
  if(i > 0) then
    result := Copy(result, 1 , i -1);
end;

function HasValue(value:string): Boolean;
var
  name: string;
begin
  Result := IsSwitch(value);
  if(Result) then
  begin
    name := SwitchName(value);
    Result := (Length(value) > length(name) + 1) and (value[Length(name)+2] = ':');
  end;
end;

function Switch(aSwitchName: string):Boolean;
var
  i: integer;
  s:string;
begin
  aSwitchName := LowerCase(aSwitchName);
  for i := 1 to ParamCount do
  begin
    s := LowerCase(ParamStr(i));

    if IsSwitch(s) and (SwitchName(s) = aSwitchName) then
    begin
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;

function SwitchValue(aSwitchName: string): string;
var
  i: integer;
  s:string;
begin
  aSwitchName := LowerCase(aSwitchName);
  for i := 1 to ParamCount do
  begin
    s := LowerCase(ParamStr(i));
    if IsSwitch(s) and (aSwitchName = SwitchName(s)) then
    begin
      if not HasValue(s) then Exit('');
      Exit(Copy(s,Length(aSwitchName)+3, 100));
    end;
  end;
  Result := '';
end;

end.
