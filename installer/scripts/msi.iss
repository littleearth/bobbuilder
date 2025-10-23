// Usage
//
// ---
// [Setup]
// CreateUninstallRegKey=not IsMSI
//
//
// Do not include innosetup uninstall
// [Icons]
// Name: "{group}\Uninstall {#AppName}"; Filename: "{uninstallexe}"; Check: IsNotMSI
//
// [Registry]
// Root: HKLM; Subkey: "SOFTWARE\Inno Setup MSIs"; Check: IsMSI; AfterInstall: PrepareMSIUninstall
//
// [Code]
// #include "scripts\msi.iss"

function IsMSI(): Boolean;
var
  x: Integer;
begin
  Result := False;

  for x := 1 to ParamCount do
  begin
    if CompareText(Copy(ParamStr(x), 1, 4), '/MSI') = 0 then
    begin
      Result := True;
    end;
  end;
end;

function IsNotMSI(): Boolean;
begin
  Result := not IsMSI;
end;

procedure PrepareMSIUninstall();
var
  x: Integer;
  subkey: String;
begin
  for x := 1 to ParamCount do
  begin
    if CompareText(Copy(ParamStr(x), 1, 5), '/MSI=') = 0 then
    begin
      subkey := 'SOFTWARE\Inno Setup MSIs\' + Copy(ParamStr(x), 6, Length(ParamStr(x)) - 5);
      RegDeleteKeyIncludingSubkeys(HKLM, subkey);
      RegWriteStringValue(HKLM, subkey, '', ExpandConstant('{uninstallexe}'));
    end;
  end;
end;