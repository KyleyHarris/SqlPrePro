(*

  This source code is Copyright (C) 2000  Kyley Harris

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the author be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone who has received permission from the author
  in writing, or purchased a licence to use from the author to use this
  software for any purpose, including commercial applications, and to alter it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
  4. The source codes may not be redistributed without permission.
  5. This header must not be removed or altered.

  Any changes to the code would be appreciated to be emailed to the author
  for consideration to any improvements.

  Kyley Harris, Auckland, New Zealand
  Kyley@HarrisSoftware.com

*)
unit uTemplate;

interface

uses
  sysutils,
  classes;

const BLANK_PARAM='_INSERTBLANK_';
type

  TCodeStackItem = class(TObject)
  private
    FReplaceValue: string;
    FParamName: string;
    procedure SetParamName(const Value: string);
    procedure SetReplaceValue(const Value: string);
  public
    property ParamName:string read FParamName write SetParamName;
    property ReplaceValue:string read FReplaceValue write SetReplaceValue;
  end;

  TCodeStackReplace = class(TObject)
  private
    FParamPos: integer;
    FParam: TCodeStackItem;
    procedure SetParam(const Value: TCodeStackItem);
    procedure SetParamPos(const Value: integer);
  public
    property Param:TCodeStackItem read FParam write SetParam;
    property ParamPos:integer read FParamPos write SetParamPos;
  end;

  TStackReplace = class(TComponent)
  protected
    FParams:TStringList;
    FStack:TList;
  public
    constructor Create(AOwner:TComponent);override;
    procedure AddParam(AName,AValue:string);
    function Process(AValue:string):string;
    destructor Destroy;override;
  end;

implementation

uses
  StrUtils,
  Math;


type
  TLongestFirst = class(TStringList)
  public
    procedure LongestFirst;
  end;
{ TTemplate }



function HssStringReplace(const s:string; AFind, AReplace: string): string;
var
  i:integer;
  ASourceLen,AFindLen,AReplaceLen:integer;
  LenDif:integer;
  AMatchPos:integer;
begin
  AFind := UpperCase(AFind);
  AFindLen := Length(AFind);
  AReplaceLen := Length(AReplace);
  ASourceLen := Length(s);
  LenDif := AReplaceLen-AFindLen;

  result := s;

  if AFind = '' then
  begin
    exit;
  end else

  if ASourceLen > 0 then
  begin

    i := 1;
    AMatchPos := 1;

    repeat
      if UpCase(Result[i]) = AFind[AMatchPos] then
        Inc(AMatchPos) else
        AMatchPos := 1;

      if AMatchPos = AFindLen+1 then
      begin
        AMatchPos := 1;
        Delete(result,i-AFindLen+1,AFindLen);
        Insert(AReplace,result,i-AFindLen+1);
        Inc(i,LenDif+1);
      end else
      Inc(i);

    until i > Length(result);

  end;
end;

{ TLongestFirst }

function SortLongestFirst(List: TStringList; Index1, Index2: Integer): Integer;

begin
  if Length(List.Names[Index1]) > Length(List.Names[Index2]) then
    result := -1 else
  if Length(List.Names[Index1]) < Length(List.Names[Index2]) then
    result := 1 else
    result := 0;
end;

procedure TLongestFirst.LongestFirst;
begin
  CustomSort(SortLongestFirst);
end;

{ TStackReplace }

procedure TStackReplace.AddParam(AName, AValue: string);
var
  O:TCodeStackItem;
begin
  o := TCodeStackItem.Create;
  o.ParamName := AName;
  o.ReplaceValue := AValue;
  FParams.AddObject(UpperCase(AName), o);
end;

constructor TStackReplace.Create(AOwner: TComponent);
begin
  inherited;
  FParams := TStringList.Create;
  FParams.sorted := True;
  FStack := TList.Create;
end;

destructor TStackReplace.Destroy;
var
  i:integer;
begin
  for i := 0 to FStack.count - 1 do tobject(FStack[i]).Free;
  while FParams.Count > 0 do
  begin
    FParams.Objects[0].free;
    FParams.Delete(0);
  end;
  FreeAndNil(FParams);
  FreeAndNil(FStack);
  inherited;
end;


function SortStack(Item1, Item2: Pointer): Integer;
begin

  result := CompareValue(TCodeStackReplace(Item1).ParamPos,TCodeStackReplace(Item2).ParamPos);
end;

function TStackReplace.Process(AValue: string): string;
var
  CSR:TCodeStackReplace;
  i:integer;
  s:string;
  i1:integer;
  i2:integer;

  function GetChar(APos:integer):Char;
  begin
    if APos < 1 then
      result := #32 else
    if APos > Length(AValue) then
      result := #32 else
      result := AValue[APos];
  end;
  function WhiteSpace(APos:integer):boolean;
  begin
    result := CharInSet(GetChar(APos),[
    #0..#31,
    #32,
    ',',
    '/',
    '+',
    '.',
    '-',
    '*',
    '(',
    ')',
    '=']);
  end;
var

  p:string;
  pl:integer;
  LengthValue:integer;
begin
  LengthValue := length(AValue);
  s := uppercase(Avalue);

  for i1 := FParams.Count -1 downto 0 do
  begin
    p := uppercase(FParams[i1]);
    pl := length(p);
    i := 1;
    repeat
      i := PosEx(p,s,i);
      if (i > 0) then
      if (WhiteSpace(i-1) and WhiteSpace(i+pl)) then
      begin
        CSR := TCodeStackReplace.Create;
        CSR.Param := FParams.Objects[i1] as TCodeStackItem;
        CSR.FParamPos := i;
        for i2 := i to i + Length(FParams[i1]) -1 do
          s[i2] := ' ';
        FStack.add(CSR);
        i := i + pl -1;
      end else
        inc(i);
    until (i = 0) or (i > LengthValue);
  end;

  FStack.Sort(SortStack);
  while FStack.Count > 0 do
  begin
    CSR :=  TCodeStackReplace(FStack[FStack.count-1]);
    FStack.Delete(FStack.count-1);
    try
      Delete(AValue,CSR.ParamPos,Length(CSR.Param.FParamName));
      Insert(CSR.Param.ReplaceValue,AValue,CSR.ParamPos);
    finally
      FreeAndNil(CSR);
    end;
  end;
  result := AValue;
end;

{ TCodeStackItem }

procedure TCodeStackItem.SetParamName(const Value: string);
begin
  FParamName := Value;
end;

procedure TCodeStackItem.SetReplaceValue(const Value: string);
begin
  FReplaceValue := Value;
end;

{ TCodeStackReplace }

procedure TCodeStackReplace.SetParam(const Value: TCodeStackItem);
begin
  FParam := Value;
end;

procedure TCodeStackReplace.SetParamPos(const Value: integer);
begin
  FParamPos := Value;
end;

end.
