unit uSqlEditor;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
  Classes,
  Types,
  SynHighlighterSQL,
  SynEditRegexSearch,
  SynEditSearch,
  SynEditTypes,
  SynEdit;

type
  TJumpToDeclarationEvent = procedure(Sender: TObject; word : string)
    of object;

  { TSqlEditor }

  TSqlEditor = class(TSynEdit)
  private
    FOnJumpToDeclaration: TJumpToDeclarationEvent;
    procedure DoSearchReplaceText(AReplace, ABackwards: boolean);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SetOnJumpToDeclaration(const Value: TJumpToDeclarationEvent);
{$IFDEF FPC}
    function GetWordAtCursor : string;
{$ENDIF}
  protected
    SearchRegEx: TSynEditRegexSearch;
    Search: TSynEditSearch;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure IncreaseFontSize;
    procedure DecreaseFontSize;
    property OnMouseWheel;
{$IFDEF FPC}
    property WordAtCursor: string read GetWordAtCursor;
{$ENDIF}
    property OnJumpToDeclaration: TJumpToDeclarationEvent read FOnJumpToDeclaration write SetOnJumpToDeclaration;
  end;


implementation
uses
  dlgConfirmReplace,
  dlgSearchText,
  dlgReplaceText, Math, SysUtils, Dialogs, Controls;

procedure TSqlEditor.DecreaseFontSize;
begin
  Font.Size := Max(Font.Size - 2, 8) ;
end;

{ TSqlEditor }

procedure TSqlEditor.AfterConstruction;
begin
  inherited;
  SearchRegEx := TSynEditRegexSearch.Create(self);
  Search := TSynEditSearch.Create({$IFNDEF FPC}Self{$ENDIF});
//  AddKeyDownHandler(KeyUpHandler);
end;

procedure TSqlEditor.BeforeDestruction;
begin
  FreeAndNil(Search);
  inherited;
end;

function TSqlEditor.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;

begin
  Result := False;
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
    Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;
  bSearchFromCaret: Boolean;

resourcestring
  STextNotFound = 'Text not found';
  SNoSelectionAvailable = 'The is no selection available, search whole text?';


procedure TSqlEditor.DoSearchReplaceText(AReplace, ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin

  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not bSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
  begin
    if (not SelAvail) or SameText(SelText, gsSearchText) then
    begin
      if MessageDlg(SNoSelectionAvailable, mtWarning, [mbYes, mbNo], 0) = mrYes then
        gbSearchSelectionOnly := False
      else
        Exit;
    end
    else
      Include(Options, ssoSelectedOnly);
  end;

  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);

  if SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
{$IFDEF MSWINDOWS}
    MessageBeep(MB_ICONASTERISK);
{$ENDIF}

    if ssoBackwards in Options then
      BlockEnd := BlockBegin
    else
      BlockBegin := BlockEnd;
    CaretXY := BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TSqlEditor.IncreaseFontSize;
begin
  Font.Size := Min(Font.Size + 2, 40);
end;


procedure TSqlEditor.KeyUpHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    if Key = VK_ADD then
    begin
      IncreaseFontSize;
      Key := 0;
    end else
    if Key = VK_SUBTRACT then
    begin
      DecreaseFontSize;
      Key := 0;
    end else
    if Key = Ord('F') then
    begin
      Key := 0;
      ShowSearchReplaceDialog(False);
    end else
    if Key = Ord('H') then
    begin
      Key := 0;
      ShowSearchReplaceDialog(True);
    end;

  end else
  if Shift = [] then
  begin
    if Key = VK_F3 then
    begin
      DoSearchReplaceText(FALSE, FALSE);
      Key := 0;
    end;
    if Key = VK_F12 then
    begin
      if Assigned(FOnJumpToDeclaration) and (WordAtCursor <> '') then
        FOnJumpToDeclaration(Self, WordAtCursor);
    end;
  end;

end;

procedure TSqlEditor.SetOnJumpToDeclaration(const Value: TJumpToDeclarationEvent);
begin
  FOnJumpToDeclaration := Value;
end;

function TSqlEditor.GetWordAtCursor: string;
begin
  Result := GetWordAtRowCol(CaretXY);
end;

procedure TSqlEditor.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TTextSearchDialog;
begin

  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if SelAvail and (BlockBegin.Y = BlockEnd.Y)
      then
        SearchText := SelText
      else
        SearchText := WordAtCursor;
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    Top := Self.ClientToScreen(Point(0,5)).y;
    Left := Self.ClientToScreen(Point(Self.Width - Width - 20, 0 )).X;
    if ShowModal = mrOK then begin
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      bSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        bSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;



end.
