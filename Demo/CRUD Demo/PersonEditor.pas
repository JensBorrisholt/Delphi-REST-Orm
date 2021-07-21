unit PersonEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  PersonsU, Vcl.ExtCtrls, Vcl.StdCtrls, Data.Bind.EngExt, Vcl.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.GenData,
  Data.Bind.ObjectScope, Data.Bind.Components, Vcl.ComCtrls, Vcl.Bind.GenData;

type
  TEditorMode = (emCreate, emEdit);

  TFormPersonEditor = class(TForm)
    Panel1: TPanel;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    BindingsList1: TBindingsList;
    PrototypeBindSource1: TPrototypeBindSource;
    Label1: TLabel;
    DateTimePicker1: TDateTimePicker;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    Button1: TButton;
    Label2: TLabel;
    Memo1: TMemo;
    LinkControlToField4: TLinkControlToField;
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
    procedure ControlChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FPerson: TPersonDTO;
    FEditorMode: TEditorMode;
    FCaption: string;
    procedure SetCaption(const Value: string);

  public
    { Public declarations }
    constructor Create(aPerson: TPersonDTO); reintroduce; overload;
    property Caption: string read FCaption write SetCaption;
    property Person: TPersonDTO read FPerson;

  end;

function ShowEditor(aMode: TEditorMode; aObject: TPersonDTO = nil): TFormPersonEditor;

implementation

uses
  REST.Types,
  ORM.IRequest;

const
  EditorCaption: array [TEditorMode] of string = ('Create new customer', 'Edit customer');
{$R *.dfm}

function ShowEditor(aMode: TEditorMode; aObject: TPersonDTO = nil): TFormPersonEditor;
begin
  if aObject = nil then
    aObject := TPersonDTO.Create;

  Result := TFormPersonEditor.Create(aObject);

  with Result do
  begin
    FEditorMode := aMode;
    Caption := EditorCaption[aMode];
  end;
end;

{ TFormPersonEditor }

procedure TFormPersonEditor.ControlChanged(Sender: TObject);
begin
  if Sender is TComponent then
    TLinkObservers.ControlChanged(Sender as TComponent);
end;

constructor TFormPersonEditor.Create(aPerson: TPersonDTO);
begin
  FPerson := aPerson;
  inherited Create(Application);
end;

procedure TFormPersonEditor.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Response : TResponse;
begin
  case FEditorMode of
    emCreate:
      ;
    emEdit:
      Response := NewRequest('https://localhost:44303/Contact').Put(FPerson.Index, FPerson);

      //AddBody(FPerson, TRESTContentType.ctNone).Put(FPerson.Index, FPerson);
  end;


end;

procedure TFormPersonEditor.PrototypeBindSource1CreateAdapter(Sender: TObject; var ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TObjectBindSourceAdapter<TPersonDTO>.Create(Self, FPerson, False);
  ABindSourceAdapter.AutoEdit := True;
  ABindSourceAdapter.AutoPost := True;
end;

procedure TFormPersonEditor.SetCaption(const Value: string);
begin
  FCaption := Value;
  Panel1.Caption := FCaption;
  inherited Caption := FCaption;
end;

end.
