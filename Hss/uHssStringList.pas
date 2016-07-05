{
  Copyright © 1999 by Harris Software Ltd
  Author:Kyley Harris
  Kyley@HarrisSoftware.com
------------------------------------------------------------------------------}

unit uHssStringList;

interface

uses
  SysUtils, Classes;

type

  THssStringList = class(TStringList)
  private
    FAppendStringsIndex: integer;
    procedure SetAppendStringsIndex(const Value: integer);
  public
    function CompareStrings(const S1, S2: string): Integer; override;
    function MarkerPosition(Marker:string;AbsoluteMarker:boolean=false):integer;
    procedure InsertStrings(AStrings:TStrings;AtIndex:integer);
    procedure AppendStrings(AStrings:TStrings);

    procedure ExtractStrings(AFrom,ATO:integer;AInto:TStrings);
    procedure ExtractUpToMarker(AStrings:TStrings;Marker:string);

    procedure TrimLines;
    function InsertStringsAtMarker(AStrings:string;Marker:string;DeleteMarker:boolean=true):boolean;overload;
    function InsertStringsAtMarker(AStrings:TStrings;Marker:string;DeleteMarker:boolean=true):boolean;overload;
    property AppendStringsIndex:integer read FAppendStringsIndex write SetAppendStringsIndex;
  end;
implementation




{ THssStringList }

procedure THssStringList.AppendStrings(AStrings: TStrings);
var
  i:integer;
begin
  if AppendStringsIndex > Count then
    FAppendStringsIndex := Count;


  BeginUpdate;

  for i := AStrings.Count -1 downto 0 do
    InsertObject(FAppendStringsIndex,AStrings[i],AStrings.Objects[i]);

  Inc(FAppendStringsIndex,AStrings.Count);
  
  EndUpdate;

end;

function THssStringList.CompareStrings(const S1, S2: string): Integer;
begin
//  if CaseSensitive then
    result := CompareStr(S1,s2);
//    else
//    result := CompareTextShaPas5_a(s1,s2);
end;

procedure THssStringList.ExtractStrings(AFrom, ATO: integer; AInto: TStrings);
var
  i:integer;
begin
  Assert(AFrom >= 0);
  Assert(ATo < Count);
  Assert( ATo >= AFrom );

  for i := AFrom to ATo do
  begin
    if assigned(AInto) then
    
    AInto.Add(Strings[AFrom]);
    Delete(AFrom);
  end;

  


end;

procedure THssStringList.ExtractUpToMarker(AStrings: TStrings; Marker: string);
var
  AMarkerPosition:integer;
begin
  AMarkerPosition := MarkerPosition(Marker);
  if AMarkerPosition = -1 then
  begin
    AStrings.AddStrings(self);
    Clear;
  end else
  begin
    if AMarkerPosition >= 0 then
    ExtractStrings(0,AMarkerPosition-1,AStrings);
    Delete(0);
  end;

end;


procedure THssStringList.InsertStrings(AStrings: TStrings; AtIndex: integer);
var
  i:integer;
begin
  if AtIndex > Count then
    AtIndex := Count;


  BeginUpdate;
  for i := AStrings.Count -1 downto 0 do
    InsertObject(AtIndex,AStrings[i],AStrings.Objects[i]);

  EndUpdate;

end;


function THssStringList.InsertStringsAtMarker(AStrings: string; Marker: string;
  DeleteMarker: boolean): boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := astrings;
  InsertStringsAtMarker(sl, Marker, DeleteMarker);
  sl.Free;
end;

function THssStringList.InsertStringsAtMarker(AStrings: TStrings;
  Marker: string;DeleteMarker:boolean): boolean;
var
  AMarkerPosition:integer;
begin
  AMarkerPosition := MarkerPosition(Marker);
  result := AMarkerPosition <> -1;

  if result then
  begin
    if Assigned(AStrings) then
      InsertStrings(AStrings,AMarkerPosition+1);
    if DeleteMarker then
      Delete(AMarkerPosition);
  end;

end;

function THssStringList.MarkerPosition(Marker: string;AbsoluteMarker:boolean): integer;
var
  i:integer;
  s:string;

begin
  if not AbsoluteMarker then
    Marker := UpperCase(format('<#%s>',[Marker])) else
    Marker := Trim(UpperCase(marker));

  for i := 0 to Count -1 do
  begin
    s := Trim(Strings[i]);

    if Pos(Marker,UpperCase(s)) > 0 then
    begin
      result := i;
      exit;
    end;
  end;
  
  result := -1;

end;

procedure THssStringList.SetAppendStringsIndex(const Value: integer);
begin
  FAppendStringsIndex := Value;
end;

procedure THssStringList.TrimLines;
begin
  while (Count > 0) and (Trim(Strings[0]) = '') do
    Delete(0);
  while (Count > 0) and (Trim(Strings[Count-1]) = '') do
    Delete(Count-1);
end;


initialization

end.
