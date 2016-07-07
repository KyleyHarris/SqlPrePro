object SqlEditorMainFrm: TSqlEditorMainFrm
  Left = 0
  Top = 0
  Caption = 'SqlEditorMain'
  ClientHeight = 527
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 591
    Top = 0
    Height = 527
    Align = alRight
    ExplicitLeft = 736
    ExplicitTop = 24
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 591
    Height = 527
    ActivePage = tsSql
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object tsSql: TTabSheet
      Caption = 'SQL'
      object Splitter2: TSplitter
        Left = 0
        Top = 373
        Width = 583
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 0
        ExplicitWidth = 376
      end
      object PreviewPanel: TPanel
        Left = 0
        Top = 376
        Width = 583
        Height = 123
        Align = alBottom
        BevelOuter = bvLowered
        Caption = 'PreviewPanel'
        TabOrder = 0
        object Panel4: TPanel
          Left = 1
          Top = 1
          Width = 581
          Height = 25
          Align = alTop
          Alignment = taLeftJustify
          Caption = '  Peek into code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
    end
    object tsCompiled: TTabSheet
      Caption = 'Compiled Sql'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tsMessage: TTabSheet
      Caption = 'Messages'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 583
        Height = 499
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 594
    Top = 0
    Width = 350
    Height = 527
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 1
    object pcData: TPageControl
      Left = 1
      Top = 41
      Width = 348
      Height = 485
      ActivePage = tabDataAll
      Align = alClient
      TabOrder = 0
      OnChange = pcDataChange
      object tabDataAll: TTabSheet
        Caption = 'All Items'
        ImageIndex = 5
        object ViewAllItems: TTreeView
          Left = 0
          Top = 0
          Width = 340
          Height = 457
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          HideSelection = False
          Images = ImageList1
          Indent = 21
          ParentFont = False
          RowSelect = True
          ShowLines = False
          SortType = stText
          TabOrder = 0
          OnAdvancedCustomDrawItem = ViewAllItemsAdvancedCustomDrawItem
          OnChange = ViewAllItemsChange
          OnDblClick = ViewAllItemsDblClick
          OnEditing = ViewRelatedItemsEditing
          OnKeyPress = ViewAllItemsKeyPress
        end
      end
      object tabDataRelated: TTabSheet
        Caption = 'Related'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object ViewRelatedItems: TTreeView
          Left = 0
          Top = 38
          Width = 340
          Height = 419
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = []
          Indent = 21
          ParentFont = False
          TabOrder = 0
          OnAdvancedCustomDrawItem = ViewAllItemsAdvancedCustomDrawItem
          OnChange = ViewAllItemsChange
          OnDblClick = ViewAllItemsDblClick
          OnEditing = ViewRelatedItemsEditing
        end
        object Panel3: TPanel
          Left = 0
          Top = 0
          Width = 340
          Height = 38
          Align = alTop
          BevelOuter = bvLowered
          TabOrder = 1
          object RelatedItem: TEdit
            Left = 8
            Top = 6
            Width = 321
            Height = 27
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            ReadOnly = True
            TabOrder = 0
            Text = 'TEST'
            OnClick = RelatedItemClick
          end
        end
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 348
      Height = 40
      Align = alTop
      TabOrder = 1
      object Label1: TLabel
        Left = 10
        Top = 10
        Width = 86
        Height = 13
        Caption = 'Search Name (F4)'
      end
      object SearchName: TEdit
        Left = 102
        Top = 7
        Width = 149
        Height = 21
        TabOrder = 0
        OnChange = SearchNameChange
        OnEnter = SearchNameEnter
        OnKeyDown = SearchNameKeyDown
      end
    end
  end
  object ActionList1: TActionList
    Left = 464
    Top = 264
    object actNew: TAction
      Caption = 'New'
      ShortCut = 16462
      OnExecute = actNewExecute
      OnUpdate = actNewUpdate
    end
    object actSave: TAction
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = actSaveExecute
      OnUpdate = actSaveUpdate
    end
    object actSaveAll: TAction
      Caption = 'Save All'
      ShortCut = 49235
      OnExecute = actSaveAllExecute
      OnUpdate = actSaveAllUpdate
    end
    object actCompile: TAction
      Caption = 'Compile'
      OnExecute = actCompileExecute
    end
  end
  object ImageList1: TImageList
    Left = 360
    Top = 112
    Bitmap = {
      494C010103000500040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000949294009496
      9400636163001010100000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000021347B002941
      73000004080000000000000000000000000000000000080C1000313C5A001010
      1800000000000000000000000000000000000000000000000000000000000000
      0000636563006365630063656300636563006365630063656300636563006365
      6300636563006365630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5B6B500DEDB
      DE00848284003130310000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003959B5007BB6
      FF004A75D600101C39000000000000000000182842005279D6007BB6FF002938
      5200000000000000000000000000000000000000000000000000000000000000
      000063656300CECFCE007B797B00949694007B797B00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF006365630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000BDBEBD00D6D3
      D6007B797B00292C290000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000213894005A92
      FF00528AFF004A86FF0029499C00314DA5005286FF00528AFF005A92FF001018
      2900000000000000000000000000000000004A240000943C0000A5380000BDBE
      C60063656300CECFCE00FFFFFF0031303100FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF006365630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000073717300BDBEBD00CECB
      CE00737173002124210000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000102C8C003969
      EF003971FF00396DFF00396DFF003971FF00396DFF003975FF003965D6000004
      080000000000000000000000000000000000BD590000C6712100CE751800BDBE
      BD0063656300CECFCE006B696B007B7D7B006B696B00CECFCE00CECFCE00CECF
      CE00CECFCE006365630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400C6C3C600C6C7
      C6006B696B00211C180000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010002149
      CE002159FF002155FF002155FF002155FF002159FF002961FF002949B5000000
      000000000000000000000000000000000000CE691000BD6D2100C6691000CEC7
      C600636563006365630063656300636563006365630063656300636563006365
      6300636563006365630000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084868400CECFCE00C6BE
      BD0063656300104110000030000000300000001C000000000000000000000000
      000000000000000000000000000000000000000000000000000008185A004265
      E7005A82FF003965FF000038FF000841FF001045FF00184DFF001849D600080C
      180000000000000000000000000000000000CE690800BD652100BD611000D6DF
      EF00D6BEB500C6BEBD00C6CFD600DEE3EF00CECBC600DEBA9400E7C39400DEAA
      8C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C8E8C00D6D3D600BDB6
      BD0052755A0031CB5A0031CB630018AE3900008E080000490000001800000028
      0000001400000000000000000000000000000000000008207B006B86EF008CA6
      FF00849AFF007B9AFF006B86FF000838FF000024FF00002CFF000038FF001038
      D600080C2100000000000000000000000000C6650800B5611800BD5D1000B55D
      1000BD611800BD651800BD651000BD651000BD6D1800C6792100D6862900AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C8E8C00DEDBDE00B5AE
      B5004A79520042DF730039DB730018BA3900009E000010A6290018A6390039D3
      6B00004100000000000000000000000000001030B5007B9AF70094AEFF008C9E
      FF00849AFF00849AFF008496F7008C9EF7006382F7004A69FF004A71FF007396
      FF006382E700081431000000000000000000C6650800B5551000B56D3900AD69
      3100B56D3100B56D3900B5713900B5753900BD793900BD824200D6822900AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C8E8C00E7E3E700ADA6
      AD004A7D520042E3730039D7730018B63900009E000010B2290029C352004AEB
      840000510000000000000000000000000000637DEF00BDCFFF00ADBAFF00A5B2
      FF009CAAFF0094A2F7008C9EF7008C9AF7008C9EF70094A2FF008CA2FF008CA2
      FF0094AEFF004A65BD000000000000000000C6610800AD450000CEDFEF00EFF3
      FF00E7EFF700E7EFF700E7EBF700E7E7EF00E7E7EF00B5C7DE00CE791800AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094929400EFEBEF00A5A2
      A500428652004AE77B0039D7730018B63900009E000010AE290029C352004AEB
      8400086110000000000000000000000000000028E7002149E700395DE7004261
      E7004A65D600A5B2F7009CA6EF009CA6EF0094A6F7004259BD003955BD00314D
      B500213C9C0008247B000000000000000000C6610800A53C0000D6E3EF00F7F7
      F700CECFCE00D6D3D600D6D3D600CECFCE00E7E7E700BDCBD600CE751800AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094969400F7F3F7009C9A
      9C00428E52004AEB7B0039D7730018B63900009E000010AE290029C352004AE7
      8400107521000000000000000000000000000000000000000000000000000000
      00000020C6006B82E700ADB6EF00ADB2EF005A6DCE0000000800000000000000
      000000000000000000000000000000000000C65D08009C300000DEEBEF00FFFB
      FF00DEDFDE00E7E3E700DEDFDE00DEDBDE00E7E7E700C6CFD600CE711800AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5A2A500EFEBEF009492
      9400429652004AEB840039D7730018B63900009E000010AE290029C352004AE7
      8400188A29000000000000000000000000000000000000000000000000000000
      000000000000214DE700CECBEF00C6C7EF0010288C0000000000000000000000
      000000000000000000000000000000000000C65D00009C2C0000E7EFF700FFFF
      FF00F7F3F700F7F3F700EFEBEF00E7E7E700EFEBEF00C6CFDE00CE711800AD4D
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B5B2B500F7F3F700ADA2
      A50042AA630042DF730039CB6B0029BA4A0021B2290010AE290029C352004AEB
      840021A239000000000000000000000000000000000000000000000000000000
      0000000000000028F700A5AEEF0094A2E700000C420000000000000000000000
      000000000000000000000000000000000000BD59000094240000E7F3FF00FFFF
      FF00CECBCE00D6D3D600CECFCE00CECFCE00EFEBEF00CED3DE00944D0800AD51
      1000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000949694009C9E9C008492
      8C003986520018964200189A3900189A390021A2420029AE4A0021A6420021A6
      4200108E29000000000000000000000000000000000000000000000000000000
      00000000000000000000294DF7002149DE000000000000000000000000000000
      000000000000000000000000000000000000CE690000B5450000D6E3E700E7E7
      E700EFEBE700E7E3E700DEDBDE00D6D3D600CECBCE00B5C3CE00C6590000A549
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      C3FFC78FF0030000C3FFC30FF0030000C3FFC00F0003000083FFC00F00030000
      83FFC01F00030000807FC00F000F000080078007000F000080070003000F0000
      80070003000F000080070003000F00008007F03F000F00008007F87F000F0000
      8007F87F000F00008007FCFF000F0000}
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 472
    Top = 272
  end
  object MainMenu1: TMainMenu
    Left = 232
    Top = 200
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = actNew
      end
      object Save1: TMenuItem
        Action = actSave
      end
      object SaveAll1: TMenuItem
        Action = actSaveAll
      end
      object Compile1: TMenuItem
        Action = actCompile
      end
    end
  end
end