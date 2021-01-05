unit DelphiRestOrm.ORM.Helper.GenericListHelper;

// CREDIT :  Some of the code in this unit is heavly inspired by code found in Stefan Glienke DSharp project.

interface

uses
  System.Types, System.JSON;

type
  GenericListHelper = record
  private
{$IFDEF VER210 }
    function SplitString(const S: string; const Delimiter: Char): TStringDynArray;
{$ENDIF}
  public
    class function IsGenericList(aObject: TObject): boolean; static;
    class function GetGenericType(aGenericList: TObject): TClass; static;
    class procedure UnMarshall(aGenericList: TObject; aJSONArray: TJSONArray); static;
  end;

implementation

uses
  System.Rtti, System.SysUtils, System.Classes, System.StrUtils, System.TypInfo, System.Generics.Collections,

  REST.JsonReflect;

var
  Context: TRttiContext;

type
  TJSONUnMarshallAcess = class(TJSONUnMarshal)

  end;

  TRttiTypeHelper = class helper for TRttiType
  private
    function ExtractGenericArguments(ATypeInfo: pTypeInfo): string;
    function FindType(const AName: string; out aType: TRttiType): boolean;
  public
    function GetGenericArguments: TArray<TRttiType>;
  end;

class function GenericListHelper.GetGenericType(aGenericList: TObject): TClass;
var
  RttiTypes: TArray<TRttiType>;
begin
  if not IsGenericList(aGenericList) then
    raise Exception.Create('Provided object is either nil or is not a GenericList');

  RttiTypes := Context.GetType(aGenericList.ClassType).GetGenericArguments;
  if (Length(RttiTypes) = 0) or (not RttiTypes[0].IsInstance) then
    raise Exception.Create('Provided object is either nil or is not a GenericList');
  Result := RttiTypes[0].AsInstance.MetaclassType;
end;

class function GenericListHelper.IsGenericList(aObject: TObject): boolean;
var
  RttiType: TRttiType;
  RttiField: TRttiField;
begin
  Result := False;

  if aObject = nil then
    exit;

  RttiType := Context.GetType(aObject.ClassType);
  for RttiField in RttiType.GetFields do
    if RttiField.Visibility = TMemberVisibility.mvPrivate then
      if RttiField.FieldType.IsRecord then
        if RttiField.Name = 'FListHelper' then
          exit(True);
end;

class procedure GenericListHelper.UnMarshall(aGenericList: TObject; aJSONArray: TJSONArray);
var
  Objects: TListOfObjects;
  Obj: TObject;
begin
  with TJSONUnMarshallAcess.Create do
    try
      Objects := GetArgObjects(GetGenericType(aGenericList), aJSONArray);

      for Obj in Objects do
        TList<TObject>(aGenericList).Add(Obj);
    finally
      Free;
    end;
end;

{$IFDEF VER210 }

function TGenericListHelper.SplitString(const S: string; const Delimiter: Char): TStringDynArray;
var
  Strings: TStrings;
  i: Integer;
begin
  with TStringList.Create do
    try
      StrictDelimiter := True;
      Delimiter := Delimiter;
      DelimitedText := S;
      SetLength(Result, Count);
      for i := Low(Result) to High(Result) do
        Result[i] := Strings[i];
    finally
      Free;
    end;
end;
{$ENDIF }
{ TRttiTypeHelper }

function TRttiTypeHelper.ExtractGenericArguments(ATypeInfo: pTypeInfo): string;
var
  i: Integer;
  S: string;
begin
  S := UTF8ToString(ATypeInfo.Name);
  i := Pos('<', S);
  if i > 0 then
    Result := Copy(S, Succ(i), Length(S) - Succ(i))
  else
    Result := ''
end;

function TRttiTypeHelper.FindType(const AName: string; out aType: TRttiType): boolean;
var
  LType: TRttiType;
begin
  Result := True;

  aType := Context.FindType(AName);
  if Assigned(aType) then
    exit;

  for LType in Context.GetTypes do
    if SameText(LType.Name, AName) then
      exit;

  Result := False;
end;

function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;
var
  i: Integer;
  args: TStringDynArray;
begin
  args := SplitString(ExtractGenericArguments(Handle), ',');
  if Length(args) > 0 then
  begin
    SetLength(Result, Length(args));
    for i := 0 to Pred(Length(args)) do
      FindType(args[i], Result[i]);
  end
  else
  begin
    if Assigned(BaseType) then
      Result := BaseType.GetGenericArguments;
  end;
end;

initialization

Context := TRttiContext.Create;

end.
