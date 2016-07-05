{
  Copyright © 1999 by Harris Software Ltd
  Author:Kyley Harris
  Kyley@HarrisSoftware.com
------------------------------------------------------------------------------}
 unit uHssStringsCSV;

interface
uses
  sysutils,
  classes;

type

  THssCustomStringsCSV = class(TStringList)
  private
    FCSVDelim: char;
    procedure SetCSVString(const Value: string);
    function GetCSVString: string;
  protected
    procedure SetCSVDelim(const Value: char);virtual;
    procedure HssAddCSV(const Value:string);
    property CSVString:string read GetCSVString write SetCSVString;
    function Get(Index: Integer): string; override;

  public
    constructor Create;
    property CSVDelim:char read FCSVDelim write SetCSVDelim;
  end;

  THssStringsCSV = class(THssCustomStringsCSV)
  public
    property CSVString;
  end;
implementation

{ THssStringsCSV }

procedure THssCustomStringsCSV.SetCSVString(const Value: string);
begin
  { Clear the list }
  Clear;
  { Add the string Via CSV format }
  HssAddCSV(value);
end;

function THssCustomStringsCSV.Get(Index: Integer): string;
begin
  if Index >= Count then
    result := '' else
    result := inherited Get(index);
end;

function THssCustomStringsCSV.GetCSVString: string;
begin
  result := CommaText;
end;

procedure THssCustomStringsCSV.HssAddCSV(const Value: string);
var
  i:integer;
  sParam:string;
  ch:Char;
  InStr:Boolean;
begin
  sParam := '';
  InStr := false;
  for i := 1 to length(Value) do
  begin
    ch := Value[i];
    if  ch =
      '"' then
      begin
        inStr := not InStr;
        if (inStr) and (i > 1) and (Value[i-1] = '"') then
        begin
          sParam := sParam+ch;
        end;
      end else
      if ch = FCSVDelim then
      begin
      if Not InStr then
      begin
        inherited Add(Trim(sParam));
        sParam := '';
      end else
      begin
        sParam := sParam+ch;
      end;
      end
      else { case }
      begin
        sParam := sParam+ch;
      end;
  end;
  sParam := Trim(sParam);
  if sParam <> '' then
    inherited Add(sParam);

end;

constructor THssCustomStringsCSV.Create;
begin
  inherited;
  CSVDelim := ',';
end;


procedure THssCustomStringsCSV.SetCSVDelim(const Value: char);
begin
  FCSVDelim := Value;
end;


end.
