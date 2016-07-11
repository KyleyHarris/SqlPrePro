unit SqlEditorMain;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF FPC}
  lcltype, lclIntf,
{$ELSE}
  Types,
  ImgList,
{$ENDIF}
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  SynEdit,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  ActnList,
  Menus,
// my code
  uSqlProject,
  uTextData,
  uSqlGenerator,
// synedit code
{$IFDEF CodeComplete}
  SynCompletionProposal,
{$ENDIF}
// Others
  uSqlEditor, 
  SynHighlighterSQL;

const
  Titles :Array[TTextDataType] of string = ('None', 'Procedures','Includes', 'Macros','Functions','Views');

type


  TSqlEditorMainFrm = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsSql: TTabSheet;
    tsCompiled: TTabSheet;
    tsMessage: TTabSheet;
    pcData: TPageControl;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ActionList1: TActionList;
    actNew: TAction;
    actSave: TAction;
    actSaveAll: TAction;
    actCompile: TAction;
    SearchName: TEdit;
    Label1: TLabel;
    tabDataAll: TTabSheet;
    ViewAllItems: TTreeView;
    ImageList1: TImageList;
    tabDataRelated: TTabSheet;
    ViewRelatedItems: TTreeView;
    Memo1: TMemo;
    Panel3: TPanel;
    RelatedItem: TEdit;
    Splitter2: TSplitter;
    PreviewPanel: TPanel;
    Panel4: TPanel;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    SaveAll1: TMenuItem;
    Compile1: TMenuItem;
    New1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure actNewUpdate(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actSaveUpdate(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actSaveAllUpdate(Sender: TObject);
    procedure actSaveAllExecute(Sender: TObject);
    procedure pcDataChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure actCompileExecute(Sender: TObject);
    procedure ViewAllItemsAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure ViewAllItemsChange(Sender: TObject; Node: TTreeNode);
    procedure SearchNameChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchNameEnter(Sender: TObject);
    procedure ViewAllItemsKeyPress(Sender: TObject; var Key: Char);
    procedure ViewAllItemsDblClick(Sender: TObject);
    procedure RelatedItemClick(Sender: TObject);
    procedure ViewRelatedItemsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    FLastWordAtCursor: string;
    FGenerator: TSqlGenerator;
    FEditingItem: THsTextData;
    FSql: TSqlEditor;
    FPreviewSql: TSqlEditor;
    FProject: TSqlProject;
    FCompiledSql: TSqlEditor;
    {$IFDEF CodeComplete}
    FCodeC: TSynCompletionProposal;
    {$endif}
    FHighlighter: TSynSQLSyn;
    FProjectFolder: string;
    FTables: TStringList;
    {$ifdef CodeComplete}
    procedure CodeCExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
    {$Endif}

    procedure DoSqlFontChanged(Sender: TObject);
    procedure EnableCodeComplete;
    procedure FocusSqlEdit;
    procedure DoDeclaration(Sender: TObject; word : string);
    procedure DoEnterSql(Sender: TObject);
    procedure RebuildRelated(aTextData: THsTextData);
    procedure RebuildTree(aPattern: string);
    procedure SqlMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CompileTextData(aTextData: THsTextData);
    function CompiledSQL(aSql: string): string;
    procedure DoSqlEdit(Sender: TObject);
    procedure SetProjectFolder(const Value: string);
    function GetActiveDataType: TTextDataType;
    function GetActiveList: TTextDatas;
    function GetActiveFolder: string;
    function GetActiveItem: THsTextData;

    procedure DisplayPeek(word: string; aSwitch: boolean=False);

    procedure SetEditingItem(const Value: THsTextData);
    procedure ParameterList(AText: string; AParams, ATables, ATableAlias: TStrings);
    function GetActiveHeaderNode: TTreeNode;
    function GetNode(aTextData: THsTextData): TTreeNode;
    property ActiveDataType: TTextDataType read GetActiveDataType;

    property ActiveList: TTextDatas read GetActiveList;

    property ActiveItem: THsTextData read GetActiveItem;
    property ActiveFolder: string read GetActiveFolder;
    property EditingItem: THsTextData read FEditingItem write SetEditingItem;

    property Node[aTextData: THsTextData]: TTreeNode read GetNode;
    property ActiveHeaderNode: TTreeNode read GetActiveHeaderNode;
    { Private declarations }
  public
    property ProjectFolder: string read FProjectFolder write SetProjectFolder;
    procedure AddFilesToTextData(aFiles: TStrings; aSqlList: TTextDatas);

  end;

var
  SqlEditorMainFrm: TSqlEditorMainFrm;

implementation

uses
{$IFDEF FPC}
  LazFileUtils,
{$ELSE}
  uFileUtils,
  Math,
{$ENDIF}
  IniFiles;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TSqlEditorMainFrm.actCompileExecute(Sender: TObject);
begin
  FGenerator := TSqlGenerator.Create(FProject.Macros, FProject.Includes);
  try
    FProject.IterateAll(CompileTextData);
  finally
    FreeAndNil(FGenerator);
  end;
end;

procedure TSqlEditorMainFrm.actNewExecute(Sender: TObject);
var
  TextData: THsTextData;
begin
  TextData := ActiveList.Add;;
  TextData.SQLName := '(New Item)';
  TextData.SQL := '';
  TextData.Folder := ActiveFolder;
  ViewAllItems.Selected := ViewAllItems.Items.AddChildObject(ActiveHeaderNode, TextData.SQLName, TextData);
  EditingItem := TextData;
end;

procedure TSqlEditorMainFrm.actNewUpdate(Sender: TObject);
begin
  actNew.Enabled := ActiveDataType <> dtNone;
end;

procedure TSqlEditorMainFrm.actSaveAllExecute(Sender: TObject);
begin
  FProject.SaveToDisk;
  RebuildTree('');
  ViewAllItems.Invalidate;
end;

procedure TSqlEditorMainFrm.actSaveAllUpdate(Sender: TObject);
begin
  actSaveAll.Enabled := FProject.Modified;
end;

procedure TSqlEditorMainFrm.actSaveExecute(Sender: TObject);
var
  Item: THsTextData;
  ItemNode: TTreeNode;
begin
  Item := ActiveItem;
  Item.SaveToDisk;

  ItemNode := Node[Item];
  ItemNode.ImageIndex := 0;
  ItemNode.Text := Item.SQLName;
end;

procedure TSqlEditorMainFrm.actSaveUpdate(Sender: TObject);
begin
  actSave.Enabled := Assigned(ActiveItem) and ActiveItem.IsModified;
end;

procedure TSqlEditorMainFrm.AddFilesToTextData(aFiles: TStrings; aSqlList: TTextDatas);
var
  i: Integer;
  TextData: THsTextData;
begin
  for i := 0 to aFiles.Count - 1 do
  begin
    TextData := aSqlList.Add;
    TextData.LoadFromDisk(aFiles[i]);
  end;
end;

procedure TSqlEditorMainFrm.DisplayPeek(word: string; aSwitch: boolean);
var
  FoundItem: THsTextData;
  SqlType: TTextDataType;
  Text: string;

  i: Integer;
begin
  FPreviewSql.Text := '';

  if word = '' then
    exit;

  if word[Length(word)] = '#' then
    Delete(word, Length(word), 1);

  if FProject.TextDataByName(word, FoundItem, SqlType) then
  begin
    if aSwitch then
      EditingItem := FoundItem else
      FPreviewSql.Text := FoundItem.SQL;
  end else
  begin
    i := FTables.IndexOf(LowerCase(word));
    if i > -1 then
    begin
      Text := 'TABLE '+ FTables[i]+#13#10+TStringList(FTables.Objects[i]).Text;
      FPreviewSql.Text := Text;
    end;
  end;


end;

procedure TSqlEditorMainFrm.DoDeclaration(Sender: TObject; word: string);
begin
  DisplayPeek(word, True);
end;

procedure TSqlEditorMainFrm.DoEnterSql(Sender: TObject);
begin
  pcData.ActivePage := tabDataAll;
end;

procedure TSqlEditorMainFrm.DoSqlEdit(Sender: TObject);
begin
  if Assigned(FEditingItem) then
    FEditingItem.SQL := FSql.Text;
  Node[FEditingItem].ImageIndex := 1;
  ViewAllItems.Invalidate;

end;

procedure TSqlEditorMainFrm.DoSqlFontChanged(Sender: TObject);
begin
  FCompiledSql.Font := FSql.Font;
end;

procedure TSqlEditorMainFrm.EnableCodeComplete;
begin
{$ifdef CodeComplete}
  FCodeC := TSynCompletionProposal.Create(Self);
  FCodeC.OnExecute := CodeCExecute;

  FCodeC.AddEditor(FSql);
//  FCodeC.Columns.Add.BiggestWord := 'propertyand';
  FCodeC.Options := [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter];
  FCodeC.EndOfTokenChr := '()[]. ';
  FCodeC.TriggerChars := '#.@_';
  FCodeC.TimerInterval := 200;

{$Endif}
end;

procedure TSqlEditorMainFrm.ParameterList(AText:string;AParams: TStrings;ATables:TStrings;ATableAlias:TStrings);
var
  FSource:string;
  Token:string;
begin
  AParams.Clear;
  if Assigned(ATables) then
  ATables.Clear;

//  AText := CompiledSQL(AText);

  with TSynSQLSyn.Create(nil) do
  try
    FSource := AText;
    ResetRange;
    SetLine(FSource,1);
    while not GetEol do
    begin
      Token := GetToken;
      if (GetTokenKind = ord(tkSymbol)) then
      begin
        if (Token = ':') then
        begin
          next;
         Token := GetToken;
         if (GetTokenKind = ord(tkIdentifier)) then
          if AParams.IndexOf(GetToken) = -1 then
            AParams.Add(GetToken);
        end;

      end else
      if (GetTokenKind = ord(tkVariable)) then
      begin
          if AParams.IndexOf(Token) = -1 then
            if Token <> '@' then
              AParams.Add(Token);
      end else
      if ( (GetTokenKind = ord(tkTableName) ) or
           ( (GetTokenKind = ord(tkIdentifier)) and
           ( (FHighlighter.TableNames.IndexOf(Token) <> -1 ) or ( FProject.SqlNameExists(Token) ) )
            )
          ) and Assigned(ATables) then
      begin
        ATables.Add(Token);
        repeat
        Next;
        until GetEOL or
         (
           (GetTokenKind <> ord(tkspace)) and
           (GetTokenKind <> ord(tkComment))
         )
           ;

        Token := GetToken;
        if Assigned(atablealias) then
        if GetTokenKind = ord(tkIdentifier) then
          ATableAlias.Add(Token) else
          ATableAlias.Add('');
      end;

      Next;

    end;
  finally
    Free;
  end;

end;

{$IFDEF CodeComplete}
procedure TSqlEditorMainFrm.CodeCExecute(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
var
  Params,Tables,TableAlias:TStringList;
  i,i1:integer;

  tn:string;
  Item: THsTextData;


  procedure AddProcs;
  var
    i1: Integer;
  begin
    for i1 := 0 to FProject.Procs.Count - 1 do
    begin
      item := FProject.Procs[i1];
      FCodeC.itemList.AddObject ( 'procedure \column{}'+item.SQLName,item);
      FCodeC.insertList.AddObject(item.SQLName,item);
    end;
  end;

  procedure AddMacros;
  var
    i1: Integer;
  begin
    for i1 := 0 to FProject.Macros.Count - 1 do
    begin
      item := FProject.Macros[i1];
      FCodeC.itemList.AddObject ( 'procedure \column{}'+item.SQLName,item);
      FCodeC.insertList.AddObject(item.SQLName,item);
    end;
  end;




var
  EditorText:string;
  d: TStringList;
begin
  Params := TStringList.Create;
  try
    Tables := TStringList.Create;
    TableAlias := TStringList.Create;
    try

      if Assigned(FCodeC.Form.CurrentEditor) then
        EditorText := FCodeC.Form.CurrentEditor.Lines.Text else
        EditorText := '';

      ParameterList(EditorText, Params,Tables,TableAlias);
      FCodeC.InsertList.Clear;
      FCodeC.ItemList.Clear;
      tn := '';
      if (CurrentInput = '') and SameText(FCodeC.PreviousToken,'EXEC') then
      begin
        AddProcs;
        exit;
      end;
      if (CurrentInput = '_') then
      begin
        AddMacros;
        exit;
      end;

      if (CurrentInput = '') and (FCodeC.PreviousToken <> '') then
      begin
        tn := FCodeC.PreviousToken;
        i := FHighlighter.TableNames.IndexOf(tn);
        if i = -1 then
        begin
          i := TableAlias.IndexOf(tn);
          if i <> -1 then
            tn := Tables[i];
        end;
        if i <> -1 then
        begin
          for i1 := Tables.Count -1 downto 0 do
            if not SameText(Tables[i1],tn) then
              Tables.Delete(i1);
        end else
          tn := '';
      end;


      if tn = '' then
      begin
        AddProcs;
        for i := 0 to FProject.Views.Count - 1 do
        begin
          Item := FProject.Views[i];
          FCodeC.ItemList.AddObject ( 'view \column{}'+Item.SQLName,Item);
          FCodeC.InsertList.AddObject(Item.SQLName,Item);
        end;
        for i := 0 to FProject.Funcs.Count - 1 do
        begin
          Item := FProject.Funcs[i];
          FCodeC.ItemList.AddObject ( 'function \column{}'+Item.SQLName,Item);
          FCodeC.InsertList.AddObject(Item.SQLName,Item);
        end;

        for i := 0 to FProject.Includes.Count - 1 do
        begin
          Item := FProject.Includes[i];
          FCodeC.ItemList.AddObject ( 'function \column{}#'+Item.SQLName+'#',Item);
          FCodeC.InsertList.AddObject('#'+Item.SQLName+'#',Item);
        end;

        AddMacros;
        for i := 0 to Params.Count - 1 do
        begin
          FCodeC.ItemList.AddObject ( 'var \column{}'+Params[i],nil);
          FCodeC.InsertList.AddObject(Params[i],nil);
        end;
      end else
      begin
        for i := 0 to Tables.Count -1 do
        begin
          try
            if FHighlighter.TableNames.IndexOf(Tables[i]) = -1 then Continue;
            d := FHighlighter.TableNames.Objects[FHighlighter.TableNames.IndexOf(Tables[i])] as TStringList;

            for i1 := 0 to d.count-1 do
            begin
              FCodeC.InsertList.AddObject(d[i1], nil);
              FCodeC.ItemList.AddObject ( 'property \column{}'+Tables[i]+'.\style{+B}'+d[i1]+'\style{-B}',nil);
            end;
          except
          end;
        end;

      end;

      FCodeC.ItemList.AddObject ( 'EmptyId',nil);
      FCodeC.InsertList.AddObject ( 'EmptyId',nil);

      if tn = '' then
      begin
        for i := 0 to FHighlighter.TableNames.Count -1 do
        begin
          FCodeC.ItemList.AddObject ( 'TABLE \column{}\style{+B}'+FHighlighter.TableNames[i]+'\style{-B}',FHighlighter.TableNames.Objects[i]);
          FCodeC.InsertList.AddObject(FHighlighter.TableNames[i],FHighlighter.TableNames.Objects[i]);
        end;
        for i := 0 to TableAlias.Count -1 do
        begin
          if TableAlias[i] = '' then
            Continue;
          FCodeC.ItemList.AddObject ( 'Alias \column{}\style{+B}'+TableAlias[i]+'\style{-B}',nil);
          FCodeC.InsertList.AddObject(TableAlias[i],nil);
        end;
      end;
      FCodeC.ResetAssignedList;
    finally
      FreeAndNil(Tables);
      FreeAndNil(TableAlias);
    end;

  finally
    FreeAndNil(Params);
  end;

end;
{$EndIf}

function TSqlEditorMainFrm.CompiledSQL(aSql: string): string;
begin
  with TSqlGenerator.Create(FProject.Macros, FProject.Includes) do
  try
    Result := CompileSql(aSQL);
  finally
    Free;
  end;
end;

procedure TSqlEditorMainFrm.CompileTextData(aTextData: THsTextData);
begin
  if aTextData.SqlType in [dtMacro, dtInclude] then
    exit;

  Memo1.Clear;
  with TStringList.Create do
  try
    try
      Text := FGenerator.CompileSql(aTextData.SQL);
      SaveToFile(FProject.ProjectFolder + '\Compiled\'+aTextData.SqlName+'.sql');
    except
      on e:exception do
      begin
        Text := 'ERROR Processing '+aTextData.SQLName+#13#10+e.Message+#13#10+StringOfChar('-',40)+#13#10;
        Memo1.Lines.Add(Text);
        PageControl1.ActivePage := tsMessage;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TSqlEditorMainFrm.FocusSqlEdit;
begin
  PageControl1.ActivePage := tsSql;
  FSql.SetFocus;
end;

procedure TSqlEditorMainFrm.FormCreate(Sender: TObject);
begin
  FTables := TStringList.Create;

  FProject := TSqlProject.Create;
  FHighlighter := TSynSQLSyn.Create(Self);
  FHighlighter.CommentAttri.Foreground := clGreen;
  FHighlighter.IdentifierAttri.Foreground := clGreen;
  FHighlighter.IdentifierAttri.Style := [fsBold];
  FHighlighter.KeyAttri.Foreground := clBlue;
  FHighlighter.KeyAttri.Style := [fsBold];
  FHighlighter.StringAttri.Foreground := clRed;
  FHighlighter.StringAttri.Style := [fsBold];
  FHighlighter.SymbolAttri.Style := [fsBold];
  FHighlighter.TableNameAttri.Foreground := clBlack;
  FHighlighter.TableNameAttri.Style := [fsBold];
  FHighlighter.SQLDialect := sqlMSSQL2K;
  FSql := TSqlEditor.Create(self);
  FSql.OnEnter := DoEnterSql;
  FSql.Parent := tsSql;
  FSql.Align := alClient;
  FSql.OnChange := DoSqlEdit;
  FSql.OnMouseWheel := SqlMouseWheel;
  FSql.OnJumpToDeclaration := DoDeclaration;
  FSql.Highlighter := FHighlighter;

  EnableCodeComplete;

  FCompiledSql := TSqlEditor.Create(self);
  FCompiledSql.ReadOnly := True;
  FCompiledSql.Parent := tsCompiled;
  FCompiledSql.Align := alClient;
  FCompiledSql.Highlighter := FHighlighter;
  FCompiledSql.OnMouseWheel := SqlMouseWheel;

  FPreviewSql := TSqlEditor.Create(self);
  FPreviewSql.ReadOnly := True;
  FPreviewSql.Parent := PreviewPanel;
  FPreviewSql.Align := alClient;
  FPreviewSql.Highlighter := FHighlighter;
  FPreviewSql.OnMouseWheel := SqlMouseWheel;

  if ParamStr(1) <> '' then
    ProjectFolder := ParamStr(1) else
    ProjectFolder := ExtractFilePath(ParamStr(0))+'SampleProject';


  PageControl1.ActivePage := tsSql;
  pcData.ActivePage := tabDataAll;
end;

procedure TSqlEditorMainFrm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  FreeAndNil(FProject);
  for i := 0 to FTables.Count - 1 do
    FTables.Objects[i].Free;
  FreeAndNil(FTables);
end;

procedure TSqlEditorMainFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F4 then
  begin
    Key := 0;
    SearchName.SetFocus;
    SearchName.Text := '';
  end else
  if Key = VK_F6 then
  begin
    Key := 0;
    FocusSqlEdit;
  end;
end;

function TSqlEditorMainFrm.GetActiveDataType: TTextDataType;
var
  Node: TTreeNode;
  i: TTextDataType;
begin
  Node := ActiveHeaderNode;
  if Node = nil then
  begin
    Result := dtNone;
    exit;
  end;
  begin
    for I := Low(Titles) to High(Titles) do
      if SameText(Titles[i], Node.Text) then
      begin
        Result := i;
        exit;
      end;
  end;
  Result := dtNone;
end;

function TSqlEditorMainFrm.GetActiveFolder: string;
begin
  Result := FProject.Folder[ActiveDataType];
end;

function TSqlEditorMainFrm.GetActiveHeaderNode: TTreeNode;
begin
  Result := ViewAllItems.Selected;
  if assigned(Result) and (Result.Data <> nil) then
    Result := Result.Parent;
end;

function TSqlEditorMainFrm.GetActiveItem: THsTextData;
begin
  Result := nil;
  if assigned(ViewAllItems.Selected) then
  begin
    Result := THsTextData(ViewAllItems.Selected.Data);
  end;
end;

function TSqlEditorMainFrm.GetActiveList: TTextDatas;
begin
  Result := FProject.SqlList[ActiveDataType];
end;

function TSqlEditorMainFrm.GetNode(aTextData: THsTextData): TTreeNode;
begin
  with ViewAllItems.Items.GetEnumerator do
  try
    while MoveNext do
    begin
      if Current.Data = aTextData then
      begin
        Result := Current;
        Exit;
      end;
    end;
    Result := nil;
  finally
    Free;
  end;
end;

procedure TSqlEditorMainFrm.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePage = tsCompiled then
  begin
    DoSqlFontChanged(nil);
    FCompiledSql.Text := '';
    if Assigned(ActiveItem) then
    begin
      try
        FCompiledSql.Text := CompiledSQL(ActiveItem.SQL);
      except
        on E:Exception do
          FCompiledSql.Text := 'ERROR:'#13#10 + e.Message;
      end;
    end;
  end;
end;

procedure TSqlEditorMainFrm.pcDataChange(Sender: TObject);
begin

  if pcData.ActivePage = tabDataAll then
    RebuildTree('') else
  if pcData.ActivePage = tabDataRelated then
    RebuildRelated(ActiveItem);

end;

procedure TSqlEditorMainFrm.RebuildRelated(aTextData: THsTextData);
var
  NodeTables: TTreeNode;
  NodeMacros: TTreeNode;
  NodeProcs: TTreeNode;
  NodeFuncs: TTreeNode;
  NodeIncludes: TTreeNode;
  NodeViews: TTreeNode;
  Token: string;
  FoundItem: THsTextData;
  SqlType: TTextDataType;
  Match: TList;
  Tables: TStringList;

  procedure AddChild(aNode: TTreeNode; aItem: THsTextData);
  begin
    if Match.IndexOf(aItem) = -1 then
    begin
      ViewRelatedItems.Items.AddChildObject(aNode, aItem.SQLName, aItem) ;
      Match.Add(aItem);
    end;
  end;
begin
  ViewRelatedItems.Items.Clear;
  if aTextData = nil then
  begin
    RelatedItem.Text := 'No Item Selected';
    Exit;
  end;
  RelatedItem.Text := aTextData.SqlName;
  RelatedItem.Tag := Integer(aTextData);

  NodeTables := ViewRelatedItems.Items.AddChild(nil,'Model Tables/Views');
  NodeMacros := ViewRelatedItems.Items.AddChild(nil,'Macros');
  NodeProcs := ViewRelatedItems.Items.AddChild(nil,'Procedures');
  NodeFuncs := ViewRelatedItems.Items.AddChild(nil,'Functions');
  NodeIncludes := ViewRelatedItems.Items.AddChild(nil,'Includes');
  NodeViews := ViewRelatedItems.Items.AddChild(nil,'Views');

  Match := TList.Create;
  Tables := TStringList.Create;
  with TSynSQLSyn.Create(nil) do
  try
    ResetRange;
    SetLine(aTextData.SQL,1);

    while not GetEol do
    begin
      Token := GetToken;
      if (not SameText(aTextData.SQLName, Token)) and  (GetTokenKind = ord(tkIdentifier)) then
      begin
        if Token[Length(Token)] = '#' then
          Delete(Token, Length(Token), 1);

        if FProject.TextDataByName(Token, FoundItem, SqlType) then
        begin
          case SqlType of
            dtNone: ;
            dtProc: AddChild(NodeProcs, FoundItem);
            dtInclude:AddChild(NodeIncludes, FoundItem);
            dtMacro: AddChild(NodeMacros, FoundItem);
            dtFunc:AddChild(NodeFuncs, FoundItem);
            dtView:AddChild(NodeViews, FoundItem);
          end;
        end else
        if FTables.IndexOf(LowerCase(Token)) > -1 then
        begin
          if Tables.IndexOf(LowerCase(Token)) = -1 then
          begin
            ViewRelatedItems.Items.AddChild(NodeTables, Token);
            Tables.Add(LowerCase(Token));
          end;
        end;
      end;

      Next;

    end;
  finally
    Free;
    Match.Free;
    Tables.Free;
  end;

  NodeMacros.Expanded := True;
  NodeIncludes.Expanded := True;
  NodeViews.Expanded := True;
  NodeFuncs.Expanded := True;
  NodeProcs.Expanded := True;
  NodeTables.Expanded := True;


end;

procedure TSqlEditorMainFrm.RebuildTree(aPattern: string);
var
  dt: TTextDataType;
  List: TTextDatas;
  i:Integer;
  Master: TTreeNode;
  NewNode: TTreeNode;
  FirstNode: TTreeNode;
begin
  ViewAllItems.Items.Clear;

  FirstNode := nil;
  aPattern := LowerCase(aPattern);

  for dt := Succ(dtNone) to High(TTextDataType) do
  begin
    List := FProject.SqlList[dt];
    Master := ViewAllItems.Items.AddChild(nil, Titles[dt]);
    Master.ImageIndex := 0;
    for i := 0 to List.Count - 1 do
    begin
      if (aPattern = '') or (pos(aPattern, LowerCase(List[i].SQLName)) > 0) then
      begin
        NewNode := ViewAllItems.Items.AddChildObject(Master,List[i].SQLName, List[i]);
        NewNode.ImageIndex := 0;
        if FirstNode = nil then
          FirstNode := NewNode;
      end;
    end;
    Master.Expanded := True;
  end;


  ViewAllItems.Selected := FirstNode;



end;

procedure TSqlEditorMainFrm.RelatedItemClick(Sender: TObject);
begin
  EditingItem := THsTextData(RelatedItem.Tag);
end;

procedure TSqlEditorMainFrm.SearchNameChange(Sender: TObject);
begin
  RebuildTree(SearchName.Text);
end;

procedure TSqlEditorMainFrm.SearchNameEnter(Sender: TObject);
begin
  pcData.ActivePage := tabDataAll;
  
end;

procedure TSqlEditorMainFrm.SearchNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DOWN then
  begin
    Key := 0;
    pcData.ActivePage := tabDataAll;
    ViewAllItems.SetFocus;
  end else
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    SearchName.Text := '';
    RebuildTree('');
    pcData.ActivePage := tabDataAll;
    ViewAllItems.SetFocus;
  end else

end;

procedure TSqlEditorMainFrm.SetEditingItem(const Value: THsTextData);
begin
  if Assigned(FEditingItem) then
    FEditingItem.SQLName := FEditingItem.ActiveItemName;

  FEditingItem := Value;
  if Assigned(FEditingItem) then
  begin
    FSql.Text := FEditingItem.SQL;
    FSql.ReadOnly := False;
  end else
  begin
    FSql.Text := '';
    FSql.ReadOnly := True;
  end;

  if Node[FEditingItem] <> ViewAllItems.Selected then
    ViewAllItems.Selected := Node[FEditingItem];
  
end;

procedure TSqlEditorMainFrm.SetProjectFolder(const Value: string);
var
  Ini: TIniFile;
  I: Integer;
begin
  FProjectFolder := Value;
  FProject.ProjectFolder := Value;
  if FileExistsUTF8(FProject.ProjectFolder+'\Tables.Txt') then
  begin
    Ini := TIniFile.Create(FProject.ProjectFolder+'\Tables.Txt');
    try
      Ini.ReadSections(FTables);
      FTables.CaseSensitive := False;

      FHighlighter.TableNames.Assign(FTables);
      for I := 0 to FTables.Count - 1 do
      begin
        FHighlighter.TableNames.Objects[i] := TStringList.Create;
        FTables.Objects[i] := FHighlighter.TableNames.Objects[i];
        Ini.ReadSection(FHighlighter.TableNames[i],FHighlighter.TableNames.Objects[i] as TStringList);
      end;
    finally
      FreeAndNil(Ini);
    end;
  end;
  RebuildTree('');
end;

procedure TSqlEditorMainFrm.SqlMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Shift = [ssCtrl] then
  begin
    if WheelDelta < 0 then
      FSql.IncreaseFontSize else
      FSql.DecreaseFontSize;
    Handled := True;
  end;
end;


procedure TSqlEditorMainFrm.Timer1Timer(Sender: TObject);
var
  word: string;
begin
   if not FSql.Focused then
     exit;
     
   word := FSql.WordAtCursor;
   if not SameText(word,FLastWordAtCursor) then
   begin
     FLastWordAtCursor := word;
     // leave content there for help
     if FLastWordAtCursor <> '' then
       DisplayPeek(word);
   end;
end;

procedure TSqlEditorMainFrm.ViewAllItemsAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean);
begin
  Node.SelectedIndex := Node.ImageIndex;
  if State = [] then
  begin
    if Node.Data = nil then
      Sender.Canvas.Font.Color := clBlue else
      Sender.Canvas.Font.Color := clBlack;
  end;
  DefaultDraw := True;
  PaintImages := (Node.Data <> nil) and (Node.ImageIndex > 0);

end;

procedure TSqlEditorMainFrm.ViewAllItemsChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) then
  begin
    EditingItem := TObject(Node.Data) as THsTextData;
  end
  else
  begin
    EditingItem := nil;
  end;

  if Sender <> ViewAllItems then
    ViewAllItems.Selected := self.Node[EditingItem];
end;

procedure TSqlEditorMainFrm.ViewAllItemsDblClick(Sender: TObject);
begin
  FocusSqlEdit;
end;

procedure TSqlEditorMainFrm.ViewAllItemsKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
  begin
    if ViewAllItems.Selected.Data <> nil then
    begin
      FocusSqlEdit;
    end;
  end;
end;

procedure TSqlEditorMainFrm.ViewRelatedItemsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;


end.
