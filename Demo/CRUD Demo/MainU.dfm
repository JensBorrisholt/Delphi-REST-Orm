object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 574
  ClientWidth = 1101
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 791
    Top = 126
    Width = 48
    Height = 13
    Caption = 'Rigistered'
  end
  object ListView1: TListView
    Left = 8
    Top = -8
    Width = 777
    Height = 545
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Phone'
      end
      item
        Caption = 'Registered'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object LabeledEdit1: TLabeledEdit
    Left = 791
    Top = 16
    Width = 186
    Height = 21
    EditLabel.Width = 28
    EditLabel.Height = 13
    EditLabel.Caption = 'Index'
    TabOrder = 1
  end
  object LabeledEdit2: TLabeledEdit
    Left = 791
    Top = 59
    Width = 186
    Height = 21
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Name'
    TabOrder = 2
  end
  object LabeledEdit3: TLabeledEdit
    Left = 791
    Top = 99
    Width = 186
    Height = 21
    EditLabel.Width = 30
    EditLabel.Height = 13
    EditLabel.Caption = 'Phone'
    TabOrder = 3
  end
  object DateTimePicker1: TDateTimePicker
    Left = 791
    Top = 145
    Width = 186
    Height = 21
    TabOrder = 4
  end
  object Button1: TButton
    Left = 791
    Top = 192
    Width = 75
    Height = 25
    Action = actEdit
    TabOrder = 5
  end
  object Button2: TButton
    Left = 791
    Top = 223
    Width = 75
    Height = 25
    Action = actInsert
    TabOrder = 6
  end
  object Button3: TButton
    Left = 791
    Top = 254
    Width = 75
    Height = 25
    Action = actDelete
    TabOrder = 7
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = False
    AutoPost = True
    FieldDefs = <
      item
        Name = 'Index'
        FieldType = ftInteger
        Generator = 'Integers'
        ReadOnly = False
      end
      item
        Name = 'Name'
        ReadOnly = False
      end
      item
        Name = 'Phone'
        ReadOnly = False
      end
      item
        Name = 'Registered'
        FieldType = ftDateTime
        Generator = 'DateTime'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = PrototypeBindSource1CreateAdapter
    Left = 504
    Top = 180
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 492
    Top = 253
    object LinkListControlToField1: TLinkListControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Index'
      Control = ListView1
      FillExpressions = <
        item
          SourceMemberName = 'Name'
          ControlMemberName = 'SubItems[0]'
        end
        item
          SourceMemberName = 'Phone'
          ControlMemberName = 'SubItems[1]'
        end
        item
          SourceMemberName = 'Registered'
          ControlMemberName = 'SubItems[2]'
        end>
      FillHeaderExpressions = <>
      FillBreakGroups = <>
    end
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Index'
      Control = LabeledEdit1
      Track = True
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Name'
      Control = LabeledEdit2
      Track = True
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Phone'
      Control = LabeledEdit3
      Track = True
    end
    object LinkControlToField4: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Registered'
      Control = DateTimePicker1
      Track = True
    end
  end
  object AdapterBindSource1: TAdapterBindSource
    AutoActivate = True
    ScopeMappings = <>
    Left = 544
    Top = 296
  end
  object ActionList1: TActionList
    Left = 208
    Top = 288
    object actEdit: TAction
      Caption = '&Edit'
      OnExecute = actEditExecute
    end
    object actInsert: TAction
      Caption = 'Insert'
    end
    object actDelete: TAction
      Caption = 'Delete'
    end
  end
end
