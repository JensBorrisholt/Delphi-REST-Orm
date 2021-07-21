unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Data.Bind.GenData, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Generics.Collections, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,

  // ORM
  DelphiRestOrm.ORM.Helper.ObjectContainer, DelphiRestOrm.ORM.IRequest,

  // DTO
  PersonsU;

type
  TFormMain = class(TForm)
    ListView1: TListView;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LabeledEdit3: TLabeledEdit;
    LinkControlToField3: TLinkControlToField;
    Label1: TLabel;
    DateTimePicker1: TDateTimePicker;
    LinkControlToField4: TLinkControlToField;
    AdapterBindSource1: TAdapterBindSource;
    Button1: TButton;
    ActionList1: TActionList;
    actEdit: TAction;
    actInsert: TAction;
    Button2: TButton;
    Button3: TButton;
    actDelete: TAction;
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
    procedure actEditExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FPersons: TPersons;

    procedure RefreshData;
    function Selected: TPersonDTO;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.Threading,

  DelphiRestOrm.ORM.Response, PersonEditor;
{$R *.dfm}

procedure TFormMain.actEditExecute(Sender: TObject);
begin
  with ShowEditor(TEditorMode.emEdit, Selected) do
    try
      if ShowModal = mrOk then
      begin
        RefreshData;
      end;
    finally
      Free;
    end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(FPersons)
end;

procedure TFormMain.PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
begin
  FPersons := NewRequest('https://localhost:44303/Contact').BasicAuthenticator('test', 'test').GET.AsType<TPersons>;
  ABindSourceAdapter := TListBindSourceAdapter<TPersonDTO>.Create(Self, FPersons.Items, False);
  AdapterBindSource1.Adapter := ABindSourceAdapter;
  PrototypeBindSource1.Active := True;
end;

procedure TFormMain.RefreshData;
begin
//  PrototypeBindSource1.Active := False;
//  PrototypeBindSource1.Active := True;
end;

function TFormMain.Selected: TPersonDTO;
begin
  Result := TListBindSourceAdapter<TPersonDTO>(AdapterBindSource1.Adapter.GetValue).Current as TPersonDTO;
end;

end.
