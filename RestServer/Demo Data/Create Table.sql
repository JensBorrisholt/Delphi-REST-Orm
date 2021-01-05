drop table Persons
drop table #Persons

Declare @JSON varchar(max)
SELECT @JSON=BulkColumn
FROM OPENROWSET (BULK 'C:\aa\DemoData.json', SINGLE_CLOB) import
SELECT *
  into #Persons
FROM OPENJSON (@JSON)
WITH 
(
    [id] NVARCHAR(24),
    [index] INT,
    [guid] NVARCHAR(36),
    [isActive] NVARCHAR(10),
    [balance] NVARCHAR(15),
    [picture] NVARCHAR(25),
    [age] INT,
    [eyeColor] NVARCHAR(5),
    [name] NVARCHAR(21),
    [gender] NVARCHAR(10),
    [company] NVARCHAR(7),
    [email] NVARCHAR(22),
    [phone] NVARCHAR(17),
    [address] NVARCHAR(100),
    [about] NVARCHAR(MAX),
    [registered] NVARCHAR(26),
    [latitude] float,
    [longitude] float,
    [greeting] NVARCHAR(47),
    [favoriteFruit] NVARCHAR(10)
)



create table Persons
(
    [id] NVARCHAR(24) not null,
    [index] INT,
    [guid] NVARCHAR(36),
    [isActive] NVARCHAR(10),
    [balance] NVARCHAR(15),
    [picture] NVARCHAR(25),
    [age] INT,
    [eyeColor] NVARCHAR(5),
    [name] NVARCHAR(21),
    [gender] NVARCHAR(10),
    [company] NVARCHAR(7),
    [email] NVARCHAR(22),
    [phone] NVARCHAR(17),
    [address] NVARCHAR(100),
    [about] NVARCHAR(MAX),
    [registered] NVARCHAR(26),
    [latitude] float,
    [longitude] float,
    [greeting] NVARCHAR(47),
    [favoriteFruit] NVARCHAR(10),
	CONSTRAINT PK_Person PRIMARY KEY (ID)
)


insert into Persons
  select * from #Persons