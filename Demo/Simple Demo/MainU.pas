unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  DelphiRestOrm.ORM.Helper.ObjectContainer;

type
  TForm59 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form59: TForm59;

implementation

uses
  System.Generics.Collections,
  DelphiRestOrm.ORM.IRequest, DelphiRestOrm.ORM.Response, DelphiRestOrm.ORM.Helper.Azure,

  RootU, PersonU;
{$R *.dfm}

procedure TForm59.FormCreate(Sender: TObject);
var
  Container: TObjectContainer;
//  AzureAuthenticator: TAzureAuthenticator;
  Person: TPerson;
  Persons: TObjectList<TPerson>;
  s: string;
  Root: TPersons;
begin
  Memo1.Clear;
  Memo1.Lines.Add('** Get all **');
  Memo1.Lines.Add('');

  s := NewRequest('https://localhost:44303/Contact').Credentials('test', 'test').GET.ToString;
  Memo1.Lines.Add(s.Substring(0, 1000) + ' ...');

  Memo1.Lines.Add('');
  Memo1.Lines.Add('** Get single **');
  Memo1.Lines.Add('');

  s := NewRequest('https://localhost:44303/Contact').GET(8).ToString;
  Memo1.Lines.Add(s);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('** Get All AsType **');
  Memo1.Lines.Add('');

  Persons := NewRequest('https://localhost:44303/Contact').ObjectContainer(Container).Credentials('test', 'test').GET.AsType<TObjectList<TPerson>>;
  Memo1.Lines.Add('(Random) Person.Address :' + Persons[Random(Persons.Count)].Address);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('** Get Single AsType **');
  Memo1.Lines.Add('');

  Person := NewRequest('https://localhost:44303/Contact').ObjectContainer(Container).Credentials('test', 'test').GET(5).AsType<TPerson>;
  Memo1.Lines.Add('Person.Address :' + Person.Address);


  Memo1.Lines.Add('');
  Memo1.Lines.Add('** TJsonDTO test **');
  Memo1.Lines.Add('');
  Root := NewRequest('https://localhost:44303/Contact').ObjectContainer(Container).Credentials('test', 'test').GET.AsType<TPersons>;
  Memo1.Lines.Add('(Random) Person.Address :' + Persons.Items[Random(Root.Items.Count)].Address);
  (*
Replace with your own Azure credentials
  AzureAuthenticator := TAzureAuthenticator.Create('aTenantID', 'aClientID', 'aClientSecret', 'aResourceId');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('** Azure test **');
  Memo1.Lines.Add('');
  s := NewRequest('<URL>').Param(17794452).QueryParam('country', 'DK').AzureCredentials(AzureAuthenticator).GET.ToString;
  Memo1.Lines.Add(s);

  Memo1.Lines.Add('');
  Memo1.Lines.Add('** Azure test async **');
  Memo1.Lines.Add('');

  NewRequestAsync('<URL>',
      procedure(const Response: TResponse)
    begin
      Memo1.Lines.Add(Response.ToString)
    end).Param(17794452).QueryParam('country', 'DK').AzureCredentials((aTenantID', 'aClientID', 'aClientSecret', 'aResourceId').GET;
    *)
  Container.Free;
end;

end.
