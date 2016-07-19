unit SynSQLEditor;

interface

uses
  Classes,
  Controls,
  SynEdit,
  SynEditKbdHandler;

const
  TSynTabChar = #9;
  TSynValidStringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  TSynWordBreakChars = ['.', ',', ';', ':', '"', '''', '!', '?', '[', ']', '(',
                        ')', '{', '}', '^', '=', '+', '-', '*', '/', '\','|'];
type
  TSynIdentChars = set of char;

  TDisplayCoord = record
    Column: integer;
    Row: integer;
  end;

  TCustomSQLEditor = class(TCustomSynEdit)
  private
    FFocusList: TList;
    FKbdHandler: TSynEditKbdHandler;
    function GetWordAtCursor : string;
    function WordEndEx(const ACoord: TPoint): TPoint;
    procedure PrepareIdentChars(out IdentChars, WhiteChars: TSynIdentChars);
    function GetDisplayXY: TDisplayCoord;
    function BufferToDisplayPos(const ACoord: TPoint): TDisplayCoord;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure SetFocus; override;
    procedure AddFocusControl(aControl: TWinControl);
    procedure RemoveFocusControl(aControl: TWinControl);
    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);

    function WordEnd: TPoint;

    property WordAtCursor: string read GetWordAtCursor;
    property DisplayXY: TDisplayCoord read GetDisplayXY;
  end;


implementation

uses
  SysUtils,
  SynEditMiscProcs;

procedure TCustomSQLEditor.AfterConstruction;
begin
  inherited;
  FFocusList := TList.Create;
  FKbdHandler := TSynEditKbdHandler.Create;
end;

procedure TCustomSQLEditor.BeforeDestruction;
begin
  FreeAndNil(FKbdHandler);
  FreeAndNil(FFocusList);
  inherited;
end;

procedure TCustomSQLEditor.KeyPress(var Key: Char);
begin
  inherited;
  FKbdHandler.ExecuteKeyPress(Self,Key);
end;

procedure TCustomSQLEditor.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKbdHandler.ExecuteKeyUp(Self, Key, Shift);
end;

procedure TCustomSQLEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  FKbdHandler.ExecuteKeyDown( Self, Key, Shift );
end;

procedure TCustomSQLEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) and (ssDouble in Shift) then Exit;

  FKbdHandler.ExecuteMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomSQLEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  fKbdHandler.ExecuteMouseUp( Self, Button, Shift, X, Y );
end;

function TCustomSQLEditor.BufferToDisplayPos(const ACoord: TPoint): TDisplayCoord;
// BufferToDisplayPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
var
  s: string;
  i, L: integer;
  x: integer;
begin
  Result := TDisplayCoord(ACoord);
  if ACoord.Y - 1 < Lines.Count then
  begin
    s := Lines[ACoord.Y - 1];
    l := Length(s);
    x := 0;
    for i := 1 to ACoord.X - 1 do begin
      if (i <= l) and (s[i] = TSynTabChar) then
        inc(x, TabWidth - (x mod TabWidth))
      else
        inc(x);
    end;

    Result.Column := x + 1;
  end;
end;

function TCustomSQLEditor.GetDisplayXY: TDisplayCoord;
begin
  Result := BufferToDisplayPos(CaretXY);
end;

function TCustomSQLEditor.WordEndEx(const ACoord: TPoint): TPoint;
var
  CX, CY: integer;
  Line: string;
  IdentChars, WhiteChars: TSynIdentChars;
begin
  CX := ACoord.X;
  CY := ACoord.Y;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then
  begin
    Line := Lines[CY - 1];

    PrepareIdentChars(IdentChars, WhiteChars);

    CX := StrScanForCharInSet(Line, CX, WhiteChars);
    // if no "whitespace" is found just position at the end of the line
    if CX = 0 then
      CX := Length(Line) + 1;
  end;

  Result.X := CX;
  Result.Y := CY;
end;

procedure TCustomSQLEditor.PrepareIdentChars(out IdentChars, WhiteChars: TSynIdentChars);
var
  WordBreakChars: TSynIdentChars;
begin
  if Assigned(Highlighter) then
  begin
    IdentChars := Highlighter.IdentChars;
    WordBreakChars := Highlighter.WordBreakChars;
  end
  else
  begin
    IdentChars := TSynValidStringChars;
    WordBreakChars := TSynWordBreakChars;
  end;

  IdentChars := IdentChars - WordBreakChars;
  WhiteChars := [#1..#255] - IdentChars;
end;

function TCustomSQLEditor.WordEnd: TPoint;
begin
  Result := WordEndEx(CaretXY);
end;

function TCustomSQLEditor.GetWordAtCursor: string;
begin
  Result := GetWordAtRowCol(CaretXY);
end;

procedure TCustomSQLEditor.AddKeyUpHandler (aHandler : TKeyEvent);
begin
  FKbdHandler.AddKeyUpHandler(aHandler);
end;

procedure TCustomSQLEditor.RemoveKeyUpHandler (aHandler : TKeyEvent);
begin
  FKbdHandler.RemoveKeyUpHandler(aHandler);
end;

procedure TCustomSQLEditor.AddKeyDownHandler (aHandler : TKeyEvent);
begin
  FKbdHandler.AddKeyDownHandler(aHandler);
end;

procedure TCustomSQLEditor.RemoveKeyDownHandler (aHandler : TKeyEvent);
begin
  FKbdHandler.RemoveKeyDownHandler(aHandler);
end;

procedure TCustomSQLEditor.AddKeyPressHandler (aHandler : TKeyPressEvent);
begin
  FKbdHandler.AddKeyPressHandler(aHandler);
end;

procedure TCustomSQLEditor.RemoveKeyPressHandler (aHandler : TKeyPressEvent);
begin
  FKbdHandler.RemoveKeyPressHandler(aHandler);
end;

procedure TCustomSQLEditor.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FKbdHandler.AddMouseCursorHandler(aHandler);
end;

procedure TCustomSQLEditor.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FKbdHandler.RemoveMouseCursorHandler(aHandler);
end;

procedure TCustomSQLEditor.AddFocusControl(aControl: TWinControl);
begin
  FFocusList.Add(aControl);
end;

procedure TCustomSQLEditor.RemoveFocusControl(aControl: TWinControl);
begin
  FFocusList.Remove(aControl);
end;

procedure TCustomSQLEditor.SetFocus;
var
  LastControl: TWinControl;
begin
  if (FFocusList.Count > 0) then
  begin
    LastControl := TWinControl(FFocusList.Last);
    if LastControl.CanFocus then
    begin
      LastControl.SetFocus;
    end;
  end
  else
  begin
    inherited;
  end;
end;


end.

