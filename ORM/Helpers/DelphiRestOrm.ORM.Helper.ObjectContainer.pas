unit DelphiRestOrm.ORM.Helper.ObjectContainer;

interface

uses
  System.Classes, System.Generics.Collections;

type
  TObjectContainer = class(TObjectList<TObject>)
  public
    function Add<T: class, constructor>: T; reintroduce; overload;
    function Add<T: class>(aObject: T): T; overload;
    function Instance<T: class>: T;
    procedure Remove<T: class>;
  end;

implementation

uses
  System.TypInfo;
{ TObjectContainer }

function TObjectContainer.Add<T>: T;
begin
  Result := Add(T.Create);
end;

function TObjectContainer.Add<T>(aObject: T): T;
begin
  inherited Add(aObject);
  Result := aObject;
end;

function TObjectContainer.Instance<T>: T;
var
  Obj: TObject;
begin
  for Obj in Self do
    if (Obj is T) then
      exit(Obj as T);
  exit(nil);
end;

procedure TObjectContainer.Remove<T>;
var
  Obj: T;
  i: Integer;
begin
  Obj := Instance<T>;
  i := IndexOf(Obj);
  if i >= 0 then
    Delete(i);
end;

end.
