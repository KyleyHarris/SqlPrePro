// This code compiles sql that includes macros and includes.
unit uSqlGenerator;

interface
uses
  uTextData,
  uHssStringsCSV,
  Classes,
  SysUtils,
  uTemplate,
  SynHighlighterSQL,
  Contnrs, uLogger;

const
  // used as the identifier for an include fragment.
  // if the fragment name is tst then $tst is typed into the editor
  IncludeTag = '$';
type

  TSqlGenerator = class(TObject)
  private
    FLog: ILogger;
    FMacros:TStringList;
    FIncludes:TStringList;
    procedure LoadMacros(aMacros: TTextDatas);
    procedure LoadIncludes(aIncludes: TTextDatas);
  public
    constructor Create(aMacros, aIncludes: TTextDatas; aLog: ILogger);
    destructor Destroy; override;
    function CompileSql(AString: string): string;
  end;

implementation

type
  THsTextDataCodeStack = class(TObject)
    InsertPos:integer;
    DeleteSize:integer;
    Data:string;
    Macro:THsTextData;
  end;

const
  LIST_SYNTAX = '%LIST';
  MACRO_KEYWORD = 'MACRO';
  MACRO_LENGTH : Integer = Length(MACRO_KEYWORD);
  BREAK_LENGTH : Integer = Length(sLineBreak);

function ReplaceNoCase(const s,AFind,AReplace:string):string;
begin
  result := StringReplace(s,afind,areplace,[rfReplaceAll,rfIgnoreCase]);
end;

constructor TSqlGenerator.Create(aMacros, aIncludes: TTextDatas; aLog: ILogger);
begin
  FMacros := TStringList.Create;
  FIncludes := TStringList.Create;
  FLog := aLog;
  LoadMacros(aMacros);
  LoadIncludes(aIncludes);
end;

destructor TSqlGenerator.Destroy;
begin
  FMacros.Free;
  FIncludes.Free;
  inherited;
end;

procedure TSqlGenerator.LoadIncludes(aIncludes: TTextDatas);
var
  i: Integer;
begin
  FIncludes.Clear;
  for i := 0 to aIncludes.Count - 1 do
    FIncludes.AddObject(aIncludes[i].SQLName, aIncludes[i]);
  FIncludes.CaseSensitive := False;
  FIncludes.Sorted := True;
end;

procedure TSqlGenerator.LoadMacros(aMacros: TTextDatas);
var
  i: Integer;
begin
  FMacros.Clear;
  for i := 0 to aMacros.Count - 1 do
    FMacros.AddObject(aMacros[i].SQLName, aMacros[i]);
  FMacros.CaseSensitive := False;
  FMacros.Sorted := True;
end;

function TSqlGenerator.CompileSql(AString: string): string;
var
  LoggingStack: TStringList;
  StackLevel:integer;

  function ExpandString(Value:string):string;forward;

  function ExpandMacro(Macro:THsTextData; Value:string;AStack:integer):string;
  var
    CSV:THssStringsCSV;
    AResult:string;
    AParamNames:TStringList;

    procedure LoadDefaultParams;
    var
      i:integer;
      ParamName: String;
      HasParam: Boolean;
    begin
      i := 0;
      repeat
        inc(i);
        ParamName := Format('%%d',[i]);
        HasParam := pos(ParamName,aresult) > 0;
        if HasParam then
        begin
          AParamNames.Add(ParamName);
        end;
      until (not HasParam);
    end;

    procedure LoadParamList;
    var
      iBreakPos:integer;
      s:string;
    begin
      AResult := Trim(AResult);
      iBreakPos := Pos(sLineBreak,AResult);
      if (iBreakPos < 0) then
        LoadDefaultParams else
      if Pos(MACRO_KEYWORD,uppercase(AResult))=1 then
      begin
        s := Copy(AResult,MACRO_LENGTH + 1,iBreakPos-MACRO_LENGTH);
        Delete(AResult,1,iBreakPos + BREAK_LENGTH - 1);
        AParamNames.CommaText := s;
        AParamNames.Delete(0); // remove Macro Name
      end else
        LoadDefaultParams;

    end;

    function ExpectParams:integer;
    begin
      result := AParamNames.Count;
    end;

    function ListParam:boolean;
    begin
      result := pos(LIST_SYNTAX,lowercase(AResult)) > 0;
    end;

    procedure ParamSplit;
    var
      i:integer;
      Param:string;
      NL:integer;
      QL:integer;
      QM:char;
      c:char;

      function ProcessParam(rawParam: string): string;
      begin
        Result := Trim(rawParam);
        if Copy(Result, 1, 2) = '=(' then
        begin
          Result := Trim(Copy(Result, 3, Length(Result) - 3));
        end;
  
      end;

    begin
      i := 1;
      QM := '"';
      NL := 0;
      QL := 0;
      Param := '';
      while i <= Length(Value) do
      begin
        c := Value[i];
        case c of
          ',':
          begin
            if (QL = 0) and (NL = 0) then
            begin
              CSV.Add(ProcessParam(Param));
              Param := '';
            end else
            Param := Param + c;

          end;
          '(':
          begin
            Inc(NL);
            Param := Param + c;
          end;
          ')':
          begin
            Dec(NL);
            Param := Param + c;
          end;
          '"':
          begin
            if QL = 0 then
            begin
              Inc(QL);
              QM := '"';
            end else
            if QM = '"' then
            begin
              if (i < Length(Value)) and (Value[i+1] = '"') then
                Inc(i) else
                Dec(QL);
            end;
            Param := Param + c;
          end;
          '''':
          begin
            if QL = 0 then
            begin
              Inc(QL);
              QM := '''';
            end else
            if QM = '''' then
            begin
              if (i < Length(Value)) and (Value[i+1] = '''') then
                Inc(i) else
                Dec(QL);
            end;
            Param := Param + c;
          end;
          else Param := Param + c;
        end;
        Inc(i);
      end;
      if Trim(Param) <> '' then CSV.Add(ProcessParam(Param));
    end;

  var
    i:integer;
    AParam:string;
    AInsert:string;
  begin
    aresult := Macro.SQL;
    AParamNames := TStringList.Create;
    LoadParamList;
    CSV := THssStringsCSV.Create;
    try
      ParamSplit;
      if (CSV.Count <> ExpectParams) and (not ListParam) then
        raise Exception.CreateFmt('%s (%s) incorrect params',[Macro.SQLName,Value]);


      with TStackReplace.Create(nil) do
      try
        if Pos(LIST_SYNTAX,uppercase(AResult)) > 0 then
          AddParam(LIST_SYNTAX,ExpandString(Value));
        for i := 0 to AParamNames.Count-1  do
        begin
          AParam := AParamNames[i];
          AInsert := ExpandString( CSV[i] );
          AddParam(AParam,AInsert);
        end;
        AResult := Process(AResult);
      finally
        Free;
      end;

    finally
      CSV.Free;
      FreeAndNil(AParamNames);
    end;



    result := ExpandString(aResult);

  end;

  function GetIncludeContent(aString: string): string;
  var
    List : TStrings;
    LastPos: Integer;
  begin
    Result := '';
    if (aString <> '') then
    begin
      List := TStringList.Create;
      try
        List.Text := aString;
        List.Delete(0);
        Result := List.Text;

        LastPos := Length(Result)-BREAK_LENGTH + 1;
        if Copy(Result, LastPos,BREAK_LENGTH)=sLineBreak then
          Delete(Result,LastPos,BREAK_LENGTH);
      finally
        List.Free;
      end;
    end;
  end;
  
  function ExpandString(Value:string):string;
  var
    SQL:TSynSQLSyn;
    Stack:TObjectStack;
    CodeStack:THsTextDataCodeStack;
    PrivateMacros:TTextDatas;

    procedure ProcessSourceIntoCodeStack;
    var
      s:string;
      lasttoken:string;
      iPos:integer;
      data :string;
      iMacroOpeningBracket,iMacroClosingBracket:integer;
      NestBracket:integer;
      Kind:TtkTokenKind;
    begin
      if SameText('_empty',Value ) then
      begin
        result := '';
        exit;
      end;

      s := '';

      lasttoken := '';
      SQL.ResetRange;
      SQL.SetLine(result, 1);
      while not SQL.GetEol do
      begin
        Kind := TTkTokenKind(SQL.GetTokenKind);
        lasttoken := trim(s);
        s := lowercase(SQL.GetToken);
        if Kind = tkIdentifier then
        begin
          iPos := SQL.GetTokenPos+1;

          if (FMacros.IndexOf(s) <> -1) and (LastToken <> '.' ) then
          begin

            data := s;
            SQL.Next;
            s := SQL.GetToken;

            if s = '(' then
            begin
              // we are possibly in a macro execute block, where data is the possible name of a macro

              CodeStack := THsTextDataCodeStack.Create;
              Stack.Push(CodeStack);

              CodeStack.Macro := FMacros.Objects[FMacros.IndexOf(data)] as THsTextData;
              // first character of the macro that we need to delete from
              CodeStack.InsertPos := iPos;

              NestBracket := 0;
              // Record the starting point of the macro parameter stack.
              iMacroOpeningBracket := SQL.GetTokenPos +1;

              // ProcessSourceIntoCodeStack through to find the end of the macro parameters, keeping track of nested brackets
              // so that all content is parsed in correctly.
              repeat
                SQL.Next;
                s := SQL.GetToken;
                if s = '(' then Inc(NestBracket);
                if s = ')' then Dec(NestBracket);
              until SQL.GetEol or (SQL.GetTokenKind = Ord(tkSymbol)) and ( (s=')') and (NestBracket < 0) );

              if s <> ')' then
              begin
                // if we are here then we have an unterminated macro or a block of code that
                // appears to be an unterminated macro such as Test(
                // this is reasonable to raise as the format is not valid in normal sql to have
                // this type of free flow text.
                raise Exception.Create('Unterminated Macro ');
              end
              else
              begin
                iMacroClosingBracket := SQL.GetTokenPos+1;
                CodeStack.DeleteSize := iMacroClosingBracket - CodeStack.InsertPos + 1;

                Data :=  Copy(result,
                              iMacroOpeningBracket + 1,
                              (iMacroClosingBracket-iMacroOpeningBracket-1) // Length of content including brackets, minus the brackets
                              );

                LoggingStack.Add(Format('%s(%s)',[CodeStack.Macro.SQLName, Data]));
                CodeStack.Data := ExpandMacro(CodeStack.Macro,data,StackLevel);
                LoggingStack.Delete(LoggingStack.Count-1);

              end;

            end;
          end;

        end else
        begin
          s := SQL.GetToken;
          if s[1] = IncludeTag then
          begin
            // Break down the include tag to replace the content.
            CodeStack := THsTextDataCodeStack.Create;
            CodeStack.InsertPos := SQL.GetTokenPos+1;
            Stack.Push(CodeStack);
            SQL.Next;
            data := SQL.GetToken;
            if Data = '' then
            begin
              // we just found a IncludeTag symbol and there is no replacement to be done.
              Stack.pop.Free;
            end else
            begin
              // get the size to extract from original source before inserting the new source
              CodeStack.DeleteSize := Length(data) + Length(IncludeTag);
              if FIncludes.IndexOf(data) <> -1 then
              begin
                LoggingStack.Add(Format('$%s',[data]));
                CodeStack.Data := ExpandString(GetIncludeContent((FIncludes.Objects[FIncludes.IndexOf(LowerCase(data))] as THsTextData).SQL));
                LoggingStack.Delete(LoggingStack.Count-1);
              end else
                raise Exception.Create('Unknown Fragment: '+IncludeTag+data);
            end
          end;
        end;
        SQL.Next;
      end;


    end;


    procedure ExtractLocalMacros;
    const
      localMacroStart = 'localmacro';
      localMacroEnd = 'endmacro';
    var
      s, BracketText:string;
      iPos:integer;
      iMacroOpeningBracket,iMacroClosingBracket:integer;
      Macro:THsTextData;
      Kind:TtkTokenKind;
     begin
      // Scan the code for any local macros
      SQL.ResetRange;
      SQL.SetLine(result, 1);
      while not SQL.GetEol do
      begin
        Kind := ttkTokenKind( SQL.GetTokenKind);
        s := SQL.GetToken;

        if Kind = tkIdentifier then
        begin
          iPos := SQL.GetTokenPos + 1;

          if sametext(s, localMacroStart) then
          begin
            Macro := PrivateMacros.Add;
            SQL.next;
            while trim(SQL.GetToken) = '' do SQL.Next;
            Macro.SQLName := SQL.GetToken;
            Macro.SQL := Format('%s %s',[MACRO_KEYWORD,Macro.SQLName]);

            CodeStack := THsTextDataCodeStack.Create;
            Stack.Push(CodeStack);


            CodeStack.Macro := Macro;
            CodeStack.InsertPos := iPos;
            iMacroOpeningBracket := SQL.GetTokenPos + Length(Macro.SQLName);
            repeat
              SQL.Next;
              s := SQL.GetToken;
            until SQL.GetEol or (sametext(s,localMacroEnd));

            if sametext(s,localMacroEnd) then
            begin
              iMacroClosingBracket := SQL.GetTokenPos;
              CodeStack.DeleteSize := iMacroClosingBracket - CodeStack.InsertPos + 9;
              Inc(iMacroOpeningBracket);
              Dec(iMacroClosingBracket);
              BracketText := Copy(result,iMacroOpeningBracket,iMacroClosingBracket-iMacroOpeningBracket+1);
              Macro.sql := Format('%s %s %s',[MACRO_KEYWORD,Macro.SQLName,BracketText]);
              CodeStack.Data := '';
            end
            else
            begin
              raise Exception.Create('Unterminated localMacro:'+Macro.SqlName);
            end;

          end;

        end;
        SQL.Next;
      end;


    end;
  var a:integer;
  begin
    Inc(StackLevel);
    if StackLevel > 35 then
      raise Exception.Create('SQL StackOverFlow');
    Result := Value;

    PrivateMacros := TTextDatas.Create;
    try
      // Find and extract all local macros from the source and
      SQL := TSynSQLSyn.Create(nil);
      Stack := TObjectStack.Create;
      try
        ExtractLocalMacros;
        while Stack.Count > 0 do
        begin
          CodeStack := Stack.Pop as THsTextDataCodeStack;
          system.Delete(Result,CodeStack.InsertPos,CodeStack.DeleteSize);
          system.Insert(CodeStack.Data,Result,CodeStack.InsertPos);
          FreeAndNil(CodeStack);
        end;
      finally
        FreeAndNil(SQL);
        while Stack.Count > 0 do
        begin
          Stack.Pop.Free;
        end;
        FreeAndNil(Stack);
      end;


      // Check private macro names have no conflict and insert them into the
      // general macros list for processing.
      for a := 0 to PrivateMacros.Count - 1 do
      begin
        if FMacros.IndexOf(lowercase(PrivateMacros[a].SQLName)) <> -1 then
          raise exception.create('Local Macro naming conflict:'+PrivateMacros[a].SQLName);
        FMacros.AddObject(lowercase(PrivateMacros[a].SQLName),PrivateMacros[a]) ;
      end;


      SQL := TSynSQLSyn.Create(nil);
      Stack := TObjectStack.Create;
      try
        // parse source for all macros and includes that will need to be compiled and replaced
        ProcessSourceIntoCodeStack;

        // process all code back into the result.
        while Stack.Count > 0 do
        begin
          CodeStack := Stack.Pop as THsTextDataCodeStack;
          system.Delete(Result,CodeStack.InsertPos,CodeStack.DeleteSize);
          system.Insert(CodeStack.Data,Result,CodeStack.InsertPos);
          FreeAndNil(CodeStack);
        end;

      finally
        FreeAndNil(SQL);
        while Stack.Count > 0 do
        begin
          Stack.Pop.Free;
        end;
        FreeAndNil(Stack);
      end;

      // clean up and remove all the private macros
      for a := 0 to PrivateMacros.Count - 1 do
        FMacros.Delete( FMacros.IndexOfObject(PrivateMacros[a]));
    finally
      FreeAndNil(PrivateMacros);
    end;

    Dec(StackLevel);

  end;

var
  ResultData: TStringList;  
begin
  LoggingStack := TStringList.Create;
  try
    try
      StackLevel := 0;

      ResultData := TStringList.Create;
      try
        ResultData.Text := ExpandString(AString);
        // clean out empty lines that may be left from local macros etc.
        // not that necessary, but cleaner when previewing 
        while (ResultData.Count > 1) and (Trim(ResultData[0]) = '') do
          ResultData.Delete(0);

        Result := ReplaceNoCase(ResultData.Text,'EmptyId','''00000000-0000-0000-0000-000000000000''');
      finally
        FreeAndNil(ResultData);
      end;
      
    except
      on E:Exception do
      begin
        result := 'ERROR';
        raise Exception.Create(
          'Error Message: ' + e.Message + sLineBreak +
          'Stack At Error' + sLineBreak +
          LoggingStack.Text
          );
      end;
    end;
  finally
    LoggingStack.Free;
  end;
end;

end.
