object FormPersonEditor: TFormPersonEditor
  Left = 0
  Top = 0
  Caption = 'FormPersonEditor'
  ClientHeight = 306
  ClientWidth = 1284
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
    Left = 8
    Top = 134
    Width = 48
    Height = 13
    Caption = 'Rigistered'
  end
  object Label2: TLabel
    Left = 616
    Top = 56
    Width = 29
    Height = 13
    Caption = 'About'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1284
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 0
    ExplicitWidth = 1286
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 72
    Width = 186
    Height = 21
    EditLabel.Width = 27
    EditLabel.Height = 13
    EditLabel.Caption = 'Name'
    TabOrder = 1
    Text = 'Mark Anderson'
    OnChange = ControlChanged
  end
  object LabeledEdit2: TLabeledEdit
    Left = 8
    Top = 112
    Width = 186
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEdit1'
    TabOrder = 2
    Text = 
      'ipsum mi vehicula purus, eu congue sapien orci eu est. Morbi bla' +
      'ndit volutpat ante, non ultrices eros semper at. Fusce leo leo, ' +
      'mollis id viverra a, pulvinar non diam. Praesent ornare mauris a' +
      'c magna placerat rutrum.Curabitur volutpat urna fermentum sem ia' +
      'culis ultrices. Maecenas adipiscing sapien risus, nec consectetu' +
      'r arcu. Sed in arcu iaculis felis placerat elementum vitae porta' +
      ' libero. Donec quis purus nisi, ac ultrices lorem. Proin vitae d' +
      'iam id turpis rhoncus vestibulum scelerisque vel leo. Nulla quis' +
      ' scelerisque sem. Etiam imperdiet lacinia nulla sit amet varius.' +
      ' Aliquam in interdum mauris. Praesent placerat, libero vel biben' +
      'dum malesuada, velit libero vestibulum erat, vel porttitor diam ' +
      'urna et nibh. Nullam fringilla cursus tempor. Cras eleifend male' +
      'suada lacus quis consectetur. Cum sociis natoque penatibus et ma' +
      'gnis dis parturient montes, nascetur ridiculus mus. Sed vestibul' +
      'um bibendum sodales.Mauris consectetur ipsum ac turpis aliquam i' +
      'd placerat justo ornare. Donec nisl arcu, faucibus vel dictum vi' +
      'tae, dapibus vel mauris. Sed lacinia, elit ut commodo tempor, ma' +
      'uris felis consequat nisl, sit amet laoreet est quam et tortor. ' +
      'Proin mattis interdum tempus. Etiam ac vehicula neque. Donec tem' +
      'por, velit sit'
    OnChange = ControlChanged
  end
  object LabeledEdit3: TLabeledEdit
    Left = 248
    Top = 277
    Width = 121
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEdit1'
    TabOrder = 3
  end
  object DateTimePicker1: TDateTimePicker
    Left = 8
    Top = 153
    Width = 186
    Height = 21
    Date = 44501.000000000000000000
    Time = 44501.000000000000000000
    TabOrder = 4
    OnChange = ControlChanged
  end
  object Button1: TButton
    Left = 1192
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 5
  end
  object Memo1: TMemo
    Left = 616
    Top = 75
    Width = 185
    Height = 89
    Lines.Strings = (
      '28. non accumsan leo ullamcorper '
      'in. Praesent ultricies varius dolor, at '
      'ultricies felis placerat sit amet. '
      'Curabitur bibendum, eros eu varius '
      'placerat, ipsum mi vehicula purus, '
      'eu congue sapien orci eu est. Morbi '
      'blandit volutpat ante, non ultrices '
      'eros semper at. Fusce leo leo, mollis '
      'id viverra a, pulvinar non diam. '
      'Praesent ornare mauris ac magna '
      'placerat rutrum.Curabitur volutpat '
      'urna fermentum sem iaculis ultrices. '
      'Maecenas adipiscing sapien risus, '
      'nec consectetur arcu. Sed in arcu '
      'iaculis felis placerat elementum '
      'vitae porta libero. Donec quis purus '
      'nisi, ac ultrices lorem. Proin vitae '
      'diam id turpis rhoncus vestibulum '
      'scelerisque vel leo. Nulla quis '
      'scelerisque sem. Etiam imperdiet '
      'lacinia nulla sit amet varius. Aliquam '
      'in interdum mauris. Praesent '
      'placerat, libero vel bibendum '
      'malesuada, velit libero vestibulum '
      'erat, vel porttitor diam urna et '
      'nibh. Nullam fringilla cursus tempor. '
      'Cras eleifend malesuada lacus quis '
      'consectetur. Cum sociis natoque '
      'penatibus et magnis dis parturient '
      'montes, nascetur ridiculus mus. Sed '
      'vestibulum bibendum '
      'sodales.Mauris consectetur '
      'ipsumnon accumsan leo ullamcorper '
      'in. Praesent ultricies varius dolor, at '
      'ultricies felis placerat sit amet. '
      'Curabitur bibendum, eros eu varius '
      'placerat, ipsum mi vehicula purus, '
      'eu congue sapien orci eu est. Morbi '
      'blandit volutpat ante, non ultrices '
      'eros semper at. Fusce leo leo, mollis '
      'id viverra a, pulvinar non diam. '
      'Praesent ornare mauris ac magna '
      'placerat rutrum.Curabitur volutpat '
      'urna fermentum sem iaculis ultrices. '
      'Maecenas adipiscing sapien risus, '
      'nec consectetur arcu. Sed in arcu '
      'iaculis felis placerat elementum '
      'vitae porta libero. Donec quis purus '
      'nisi, ac ultrices lorem. Proin vitae '
      'diam id turpis rhoncus vestibulum '
      'scelerisque vel leo. Nulla quis '
      'scelerisque sem. Etiam imperdiet '
      'lacinia nulla sit amet varius. Aliquam '
      'in interdum mauris. Praesent '
      'placerat, libero vel bibendum '
      'malesuada, velit libero vestibulum '
      'erat, vel porttitor diam urna et '
      'nibh. Nullam fringilla cursus tempor. '
      'Cras eleifend malesuada lacus quis '
      'consectetur. Cum sociis natoque '
      'penatibus et magnis dis parturient '
      'montes, nascetur ridiculus mus. Sed '
      'vestibulum bibendum '
      'sodales.Mauris consectetur '
      'ipsumnon accumsan leo ullamcorper '
      'in. Praesent ultricies varius dolor, at '
      'ultricies felis placerat sit amet. '
      'Curabitur bibendum, eros eu varius '
      'placerat, ipsum mi vehicula purus, '
      'eu congue sapien orci eu est. Morbi '
      'blandit volutpat ante, non ultrices '
      'eros semper at. Fusce leo leo, mollis '
      'id viverra a, pulvinar non diam. '
      'Praesent ornare mauris ac magna '
      'placerat rutrum.Curabitur volutpat '
      'urna fermentum sem iaculis ultrices. '
      'Maecenas adipiscing sapien risus, '
      'nec consectetur arcu. Sed in arcu '
      'iaculis felis placerat elementum '
      'vitae porta libero. Donec quis purus '
      'nisi, ac ultrices lorem. Proin vitae '
      'diam id turpis rhoncus vestibulum '
      'scelerisque vel leo. Nulla quis '
      'scelerisque sem. Etiam imperdiet '
      'lacinia nulla sit amet varius. Aliquam '
      'in interdum mauris. Praesent '
      'placerat, libero vel bibendum '
      'malesuada, velit libero vestibulum '
      'erat, vel porttitor diam urna et '
      'nibh. Nullam fringilla cursus tempor. '
      'Cras eleifend malesuada lacus quis '
      'consectetur. Cum sociis natoque '
      'penatibus et magnis dis parturient '
      'montes, nascetur ridiculus mus. Sed '
      'vestibulum bibendum '
      'sodales.Mauris consectetur ipsum')
    TabOrder = 6
  end
  object BindingsList1: TBindingsList
    Methods = <>
    OutputConverters = <>
    Left = 272
    Top = 69
    object LinkControlToField1: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Name'
      Control = LabeledEdit1
      Track = True
    end
    object LinkControlToField2: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Address'
      Control = LabeledEdit2
      Track = True
    end
    object LinkControlToField3: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'Registered'
      Control = DateTimePicker1
      Track = True
    end
    object LinkControlToField4: TLinkControlToField
      Category = 'Quick Bindings'
      DataSource = PrototypeBindSource1
      FieldName = 'About'
      Control = Memo1
      Track = False
    end
  end
  object PrototypeBindSource1: TPrototypeBindSource
    AutoActivate = True
    AutoPost = False
    FieldDefs = <
      item
        Name = 'Name'
        Generator = 'ContactNames'
        ReadOnly = False
      end
      item
        Name = 'About'
        FieldType = ftTStrings
        Generator = 'LoremIpsum'
        ReadOnly = False
      end
      item
        Name = 'Address'
        Generator = 'LoremIpsum'
        ReadOnly = False
      end
      item
        Name = 'Registered'
        FieldType = ftDate
        Generator = 'Date'
        ReadOnly = False
      end
      item
        Name = 'Age'
        FieldType = ftUInteger
        Generator = 'AlphaColors'
        ReadOnly = False
      end
      item
        Name = 'Balance'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Company'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Email'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'EyeColor'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'FavoriteFruit'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Gender'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Greeting'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'GuId'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Id'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Index'
        FieldType = ftUInteger
        Generator = 'UIntegers'
        ReadOnly = True
      end
      item
        Name = 'Latitude'
        FieldType = ftCurrency
        Generator = 'Currency'
        ReadOnly = False
      end
      item
        Name = 'Longitude'
        FieldType = ftCurrency
        Generator = 'Currency'
        ReadOnly = False
      end
      item
        Name = 'Phone'
        Generator = 'BitmapNames'
        ReadOnly = False
      end
      item
        Name = 'Picture'
        Generator = 'BitmapNames'
        ReadOnly = False
      end>
    ScopeMappings = <>
    OnCreateAdapter = PrototypeBindSource1CreateAdapter
    Left = 296
    Top = 152
  end
end
