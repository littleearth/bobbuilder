unit PersonTestsU;

interface

uses
  DUnitX.TestFramework, PersonU;

type

  [TestFixture]
  TPersonTest = class
  private
    FPerson: TPerson;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('Test A', '40,40')]
    [TestCase('Test B', '10,10')]
    procedure Test2(const AValue1: Integer; const AValue2: Integer);
  end;

implementation

uses
  System.DateUtils, System.SysUtils;

procedure TPersonTest.Setup;
begin
  FPerson := TPerson.Create;
end;

procedure TPersonTest.TearDown;
begin
  FPerson.Free;
end;

procedure TPersonTest.Test1;
begin
  if FileExists('failtest.txt') then
    raise Exception.Create('failtest.txt Exists');
end;

procedure TPersonTest.Test2(const AValue1: Integer; const AValue2: Integer);
var
  LDate: Tdate;
  LValue1: Integer;
begin
  LValue1 := AValue1 * -1;
  LDate := IncYear(Yesterday, LValue1);
  FPerson.DateOfBirth := LDate;
  Assert.AreEqual(AValue2, FPerson.Age);
end;

initialization

TDUnitX.RegisterTestFixture(TPersonTest);

end.
