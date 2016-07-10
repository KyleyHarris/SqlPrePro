unit uSqlGenerator;

interface
uses
  uTextData,
  uHssStringsCSV,
  Classes,
  SysUtils,
  uTemplate,
  SynHighlighterSQL,
  Contnrs;

type
  TSqlGenerator = class(TObject)
  private
    FMacros:TStringList;
    FIncludes:TStringList;
    procedure LoadMacros(aMacros: TTextDatas);
    procedure LoadIncludes(aIncludes: TTextDatas);
  public
    constructor Create(aMacros, aIncludes: TTextDatas);
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

function ReplaceNoCase(const s,AFind,AReplace:string):string;
begin
  result := StringReplace(s,afind,areplace,[rfReplaceAll,rfIgnoreCase]);
end;

constructor TSqlGenerator.Create(aMacros, aIncludes: TTextDatas);
begin
  FMacros := TStringList.Create;
  FIncludes := TStringList.Create;
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
  GlobalStack: TStringList;
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
    begin
      i := 1;
      while pos('%'+IntToStr(i),aresult) > 0 do
      begin
        AParamNames.Add('%'+IntToStr(i));
        Inc(i);
      end;
    end;

    procedure LoadParamList;
    var
      i:integer;
      s:string;
    begin
      AResult := Trim(AResult);
      i := Pos(#13#10,AResult);
      if i = -1 then
        LoadDefaultParams else
      if Pos('MACRO ',uppercase(AResult))=1 then
      begin
        s := Copy(AResult,6,i-6);
        Delete(AResult,1,i+1);
        AParamNames.CommaText := s;
        AParamNames.Delete(0); // Macro Name
      end else
        LoadDefaultParams;

    end;

    function ExpectParams:integer;
    begin
      result := AParamNames.Count;
    end;

    function ListParam:boolean;
    begin
      result := pos('%list',lowercase(AResult)) > 0;
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
        raise Exception.Create(Macro.SQLName + '( '+Value+') incorrect params');


      with TStackReplace.Create(nil) do
      try
        if Pos('%LIST',uppercase(AResult)) > 0 then
          AddParam('%List',ExpandString(Value));
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

  function Content(aString: string): string;
  begin
    with TStringList.Create do
    try
      Text := aString;
      Delete(0);
      Result := Text;
      if Copy(Result, Length(Result)-1,2)=#13#10 then
        System.Delete(Result,Length(Result)-1,2)
    finally
      Free;
    end;
  end;
  
  function ExpandString(Value:string):string;
  var
    SQL:TSynSQLSyn;
    Stack:TObjectStack;
    CodeStack:THsTextDataCodeStack;
    PrivateMacros:TTextDatas;

    procedure Loop;
    var
      s:string;
      lasttoken:string;
      iPos:integer;
      data :string;
      iMDP,iMDE:integer;
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

              CodeStack := THsTextDataCodeStack.Create;
              Stack.Push(CodeStack);

              CodeStack.Macro := FMacros.Objects[FMacros.IndexOf(data)] as THsTextData;
              CodeStack.InsertPos := iPos;

              NestBracket := 0;
              iMDP := SQL.GetTokenPos +1;
              repeat
                SQL.Next;
                s := SQL.GetToken;
                if s = '(' then Inc(NestBracket);
                if s = ')' then Dec(NestBracket);
              until SQL.GetEol or (SQL.GetTokenKind = Ord(tkSymbol)) and ( (s=')') and (NestBracket < 0) );

              if s <> ')' then
                raise Exception.Create('Unterminated Macro ') else
              begin
                iMDE := SQL.GetTokenPos+1;
                CodeStack.DeleteSize := iMDE - CodeStack.InsertPos + 1;
                Inc(iMDP);
                Dec(iMDE);
                Data :=  Copy(result,iMDP,iMDE-iMDP+1);
                GlobalStack.Add(Format('%s(%s)',[CodeStack.Macro.SQLName, Data]));
                CodeStack.Data := ExpandMacro(CodeStack.Macro,data,StackLevel);
                GlobalStack.Delete(GlobalStack.Count-1);

              end;

             end ;//else raise Exception.Create('Unterminated Macro ');

          end;

        end else
        begin
          s := SQL.GetToken;
          if s ='#' then
          begin
            CodeStack := THsTextDataCodeStack.Create;
            CodeStack.InsertPos := SQL.GetTokenPos+1;
            Stack.Push(CodeStack);
            SQL.Next;
            data := SQL.GetToken;
            if Data = '' then
            begin
              Stack.pop.Free;
            end else
            if data[length(data)] = '#' then
            begin
              CodeStack.DeleteSize := (SQL.GetTokenPos+Length(Data)) - CodeStack.InsertPos + 1 ;
              data := copy(data,1,length(data)-1);
              if FIncludes.IndexOf(LowerCase(data)) <> -1 then
              begin
                GlobalStack.Add(Format('#%s#',[data]));
                CodeStack.Data := ExpandString(Content((FIncludes.Objects[FIncludes.IndexOf(LowerCase(data))] as THsTextData).SQL));
                GlobalStack.Delete(GlobalStack.Count-1);
              end else
                raise Exception.Create('Unknown Fragment:'+data);
            end else
            begin
              Stack.pop.Free;
            end;
          end;
        end;
        SQL.Next;
      end;


    end;


    procedure LoopMacros;
    var
      s:string;
      iPos:integer;
      iMDP,iMDE:integer;
      Macro:THsTextData;
      Kind:TtkTokenKind;

    begin
      SQL.ResetRange;
      SQL.SetLine(result, 1);
      while not SQL.GetEol do
      begin
        Kind := ttkTokenKind( SQL.GetTokenKind);
        s := lowercase(SQL.GetToken);

        if Kind = tkIdentifier then
        begin
          iPos := SQL.GetTokenPos+1;

          if sametext(s,'localmacro') then
          begin
            Macro := PrivateMacros.Add;
            SQL.next;
            while trim(SQL.GetToken) = '' do SQL.Next;
            Macro.SQLName := SQL.GetToken;
            Macro.SQL := 'MACRO '+ Macro.SQLName;

            CodeStack := THsTextDataCodeStack.Create;
            Stack.Push(CodeStack);


            CodeStack.Macro := Macro;
            CodeStack.InsertPos := iPos;
            iMDP := SQL.GetTokenPos + Length(Macro.SQLName);
            repeat
              SQL.Next;

              s := SQL.GetToken;
            until SQL.GetEol or (sametext(s,'endmacro'));

            if sametext(s,'endmacro') then
            begin

              begin
                iMDE := SQL.GetTokenPos;
                CodeStack.DeleteSize := iMDE - CodeStack.InsertPos + 9;
                Inc(iMDP);
                Dec(iMDE);
                Macro.sql := 'MACRO '+Macro.SQLName+' '+ Copy(result,iMDP,iMDE-iMDP+1);
                CodeStack.Data := '';
              end;

             end else raise Exception.Create('Unterminated Macro ');

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

    SQL := TSynSQLSyn.Create(nil);
    Stack := TObjectStack.Create;
    try
      LoopMacros;
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

    for a := 0 to PrivateMacros.Count - 1 do
    begin
      if FMacros.IndexOf(lowercase(PrivateMacros[a].SQLName)) <> -1 then
        raise exception.create('Local Macro nameing conflict:'+PrivateMacros[a].SQLName);
      FMacros.AddObject(lowercase(PrivateMacros[a].SQLName),PrivateMacros[a]) ;
    end;

    SQL := TSynSQLSyn.Create(nil);
    Stack := TObjectStack.Create;
    try
      try
      Loop;
      except
   //     Loop;
        raise  ;
      end  ;
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
    for a := 0 to PrivateMacros.Count - 1 do
      FMacros.Delete( FMacros.IndexOfObject(PrivateMacros[a]));

    FreeAndNil(PrivateMacros);

    Dec(StackLevel);

  end;

begin
  GlobalStack := TStringList.Create;
  try
    try
      StackLevel := 0;
      result := ExpandString(AString);
      result := ReplaceNoCase(result,'EmptyId','''00000000-0000-0000-0000-000000000000''');
    except
      on E:Exception do
      begin
        result := 'ERROR';
        raise Exception.Create('Stack Error'#13#10+GlobalStack.Text);
      end;
    end;
  finally
    GlobalStack.Free;
  end;


end;

end.
