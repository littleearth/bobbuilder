unit Bob.Compare.Tests;

interface

uses
  DUnitX.TestFramework, Bob.Compare;

type

  [TestFixture]
  TCompareTest = class
  private
    FCompare: TCompare;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [TestCase('CompareInteger Left', '2,1,1')]
    [TestCase('CompareInteger Right', '3,4,-1')]
    [TestCase('CompareInteger Same', '3,3,0')]
    procedure CompareInteger(const AValue1: string; const AValue2: string;
      AExpectedResult: integer);

    [TestCase('CompareVersion Left', '1.1.1.2,1.1.1.0,1')]
    [TestCase('CompareVersion Right', '3.1.1.1,4.1.1,-1')]
    [TestCase('CompareVersion Extra Text', '3.1.1.1 (Build 2),4.1.1,-1')]
    [TestCase('CompareVersion Same', '3.1.1.0 (Build 2),3.1.1,0')]
    procedure CompareVersion(const AValue1: string; const AValue2: string;
      AExpectedResult: integer);

    [TestCase('CompareDateTime Left', '1/1/2022,2021-01-01,1')]
    [TestCase('CompareDateTime Right', '1/1/2022,1 Oct 2023,-1')]
    [TestCase('CompareDateTime Same', 'N,N,0')]
    procedure CompareDateTime(const AValue1: string; const AValue2: string;
      AExpectedResult: integer);
  end;

implementation

uses
  System.Math, System.SysUtils;

procedure TCompareTest.CompareVersion(const AValue1, AValue2: string;
  AExpectedResult: integer);
var
  LResult: integer;
begin
  LResult := FCompare.CompareVersion(AValue1, AValue2);
  Assert.IsTrue(SameValue(AExpectedResult, LResult),
    Format('Compare %s <> %s, Expected %d, Result: %d', [AValue1, AValue2,
    AExpectedResult, LResult]));
end;

procedure TCompareTest.Setup;
begin
  FCompare := TCompare.Create;
end;

procedure TCompareTest.TearDown;
begin
  FCompare.Free;
end;

procedure TCompareTest.CompareDateTime(const AValue1, AValue2: string;
  AExpectedResult: integer);
var
  LResult: integer;
begin
  LResult := FCompare.CompareDateTime(AValue1, AValue2);
  Assert.IsTrue(SameValue(AExpectedResult, LResult),
    Format('Compare %s <> %s, Expected %d, Result: %d', [AValue1, AValue2,
    AExpectedResult, LResult]));
end;

procedure TCompareTest.CompareInteger(const AValue1: string;
  const AValue2: string; AExpectedResult: integer);
var
  LResult: integer;
begin
  LResult := FCompare.CompareInteger(AValue1, AValue2);
  Assert.IsTrue(SameValue(AExpectedResult, LResult),
    Format('Compare %s <> %s, Expected %d, Result: %d', [AValue1, AValue2,
    AExpectedResult, LResult]));
end;

initialization

TDUnitX.RegisterTestFixture(TCompareTest);

end.
