# DelpliRestORM
Delphi Rest Orm - At simple  yet powerful ORM to query a WebService

## Syntax exampel ##

``` pascal

type
  TPerson = class
  private
   ...
    FAddress: string;
   ...
  published
    ...
    property Address: string read FAddress write FAddress;
   ...
  end;


var
  Persons: TObjectList<TPerson>;
begin
  Persons := NewRequest('https://localhost:44303/Contact').Credentials('test', 'test').GET.AsType<TObjectList<TPerson>>;
  Memo1.Lines.Text := '(Random) Person.Address :' + Persons[Random(Persons.Count)].Address;
  Persons.Free;
end;
```

With this simple syntax I make the GET- requeest to a webservice, singin using basic authentication, and then parses the result into a type strong  TObjecttList. All done in one simple line.


Features : 

This OrM can easy query a WEB Service. Featuring automatic serialization and deserialization, request and response type detection, variety of authentications and other useful features-

## Serialization ##
JSON serialization and deserialization

## Authentication ##
Basic, Azure are supported. Not enough? Write your own!

## Sync and Async ##
Variety of overloads to make synchronous and asynchronous HTTP calls

## Parameters ##
Add query, URL segment, body, form or header parameter using an easy and fluent API
