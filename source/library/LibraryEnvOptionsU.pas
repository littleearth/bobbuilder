unit LibraryEnvOptionsU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Xml.XMLIntf, Xml.XMLDom, Xml.XMLDoc,
  System.Generics.Collections, LibraryPathsU;

type
  EEvnOptionException = class(Exception);

  TNodeFoundProc = reference to procedure(ANode: IDOMNode;
    var ACancel: boolean);

  TEnvOptionNode = class(TObject)
  private
    function GetDelphiBrowsingPath: string;
    procedure SetDelphiBrowsingPath(const Value: string);
    function GetDelphiDebugDCUPath: string;
    function GetDelphiLibraryPath: string;
    procedure SetDelphiDebugDCUPath(const Value: string);
    procedure SetDelphiLibraryPath(const Value: string);
  protected
    FDelphiBrowsingPathNode: IDOMNode;
    FDelphiLibraryPathNode: IDOMNode;
    FDelphiDebugDCUPathNode: IDOMNode;
    FPropertyGroupNode: IDOMNode;
    FLibraryName: string;
    procedure SetDelphiBrowsingPathNode(const Value: IDOMNode);
    procedure SetDelphiDebugDCUPathNode(const Value: IDOMNode);
    procedure SetDelphiLibraryPathNode(const Value: IDOMNode);
    procedure SetPropertyGroupNode(const Value: IDOMNode);
    property PropertyGroup: IDOMNode read FPropertyGroupNode
      write SetPropertyGroupNode;
    property DelphiLibraryPathNode: IDOMNode read FDelphiLibraryPathNode
      write SetDelphiLibraryPathNode;
    property DelphiBrowsingPathNode: IDOMNode read FDelphiBrowsingPathNode
      write SetDelphiBrowsingPathNode;
    property DelphiDebugDCUPathNode: IDOMNode read FDelphiDebugDCUPathNode
      write SetDelphiDebugDCUPathNode;
    procedure SetLibraryName(AName: string);
  public
    constructor Create(ALibraryName: string); reintroduce;
    function AsString: string;
    property LibraryName: string read FLibraryName;
    procedure ApplyLibraryPaths(ALibraryPaths: TLibraryPaths);
    property DelphiBrowsingPath: string read GetDelphiBrowsingPath
      Write SetDelphiBrowsingPath;
    property DelphiDebugDCUPath: string read GetDelphiDebugDCUPath
      Write SetDelphiDebugDCUPath;
    property DelphiLibraryPath: string read GetDelphiLibraryPath
      Write SetDelphiLibraryPath;
  end;

  TEnvOptionNodes = class(TObjectList<TEnvOptionNode>)
  public
    function FindLibrary(
      AName: string;
      AConditionSearch: boolean = false): TEnvOptionNode;
  end;

  TEnvOptions = class(TObject)
  private
    FLog: TStringList;
    FXmlDoc: IXMLDocument;
    FEnvOptionsFileName: TFileName;
    FEnvOptionNodes: TEnvOptionNodes;
    procedure SetEnvOptionsFileName(const Value: TFileName);
    procedure Log(AMessage: string);
    function GetLog: string;
    function HasAttribute(
      AAttributes: IDOMNamedNodeMap;
      AName: string;
      var AValue: string): boolean;
    procedure FindNode(
      ANodeList: IDOMNodeList;
      AName: string;
      AOnFound: TNodeFoundProc);
    procedure LoadLibraryPaths(AEnvOptionNode: TEnvOptionNode);
    procedure AddEnvOptionNodes;
    procedure ClearEnvOptionNodes;
    procedure ProcessProjectExtensions;
    procedure RemoveAttributeFromNode(
      ANode: IDOMNode;
      AAttributeName: string);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    property EnvOptionsFileName: TFileName read FEnvOptionsFileName
      write SetEnvOptionsFileName;
    property LogText: string read GetLog;
    function BackupCurrentConfig: boolean;
    function FindLibrary(AName: string): TEnvOptionNode;
    procedure ApplyLibraryPaths(
      ALibraryName: string;
      ALibraryPaths: TLibraryPaths);
    procedure Load;
    procedure Save;
    function AsXML: string;
  end;

implementation

uses
  System.StrUtils, Lazy.Utils.Windows;

{ TEnvOptions }

procedure TEnvOptions.SetEnvOptionsFileName(const Value: TFileName);
begin
  FEnvOptionsFileName := Value;
end;

function TEnvOptions.BackupCurrentConfig: boolean;
var
  LFolder: string;
  LFileName: TFileName;
begin
  LFolder := IncludeTrailingPathDelimiter(ExtractFilePath(EnvOptionsFileName) +
    'Backup');
  Log('Backing up to : ' + LFolder);
  Result := TLZFile.CheckDirectoryExists(LFolder, true);
  if Result then
  begin
    LFileName := LFolder + FormatDateTime('yyyymmddhhnnss', Now) + '-' +
      ExtractFileName(EnvOptionsFileName);
    if FileExists(EnvOptionsFileName) then
    begin
      Result := CopyFile(PChar(EnvOptionsFileName), PChar(LFileName), true);
    end;
  end;
  if not Result then
    Log('ERROR: Failed to backup to : ' + LFolder);
end;

constructor TEnvOptions.Create;
begin
  inherited;
  FLog := TStringList.Create;
  FEnvOptionNodes := TEnvOptionNodes.Create;
  FXmlDoc := TXMLDocument.Create(nil);
  AddEnvOptionNodes;
end;

destructor TEnvOptions.Destroy;
begin
  ClearEnvOptionNodes;
  FreeAndNil(FEnvOptionNodes);
  FreeAndNil(FLog);
  FXmlDoc := nil;
  inherited;
end;

function TEnvOptions.GetLog: string;
begin
  Result := FLog.Text;
end;

procedure TEnvOptions.Log(AMessage: string);
begin
  FLog.Add(AMessage);
end;

function TEnvOptions.HasAttribute(
  AAttributes: IDOMNamedNodeMap;
  AName: string;
  var AValue: string): boolean;
var
  LAttributeIdx: integer;
begin
  Result := false;
  if Assigned(AAttributes) then
  begin
    LAttributeIdx := 0;
    while (not Result) and (LAttributeIdx < AAttributes.length) do
    begin
      if SameText(AName, AAttributes[LAttributeIdx].nodeName) then
      begin
        AValue := AAttributes[LAttributeIdx].nodeValue;
        Result := true;
      end;
      Inc(LAttributeIdx);
    end;
  end;
end;

procedure TEnvOptions.RemoveAttributeFromNode(
  ANode: IDOMNode;
  AAttributeName: string);
var
  LAttributes: IDOMNamedNodeMap;
  LAttributeNode: IDOMNode;
begin
  if Assigned(ANode) and Assigned(ANode.attributes) then
  begin
    LAttributes := ANode.attributes;
    LAttributeNode := LAttributes.getNamedItem(AAttributeName);
    if Assigned(LAttributeNode) then
    begin
      LAttributes.removeNamedItem(AAttributeName);
      Log('Removed ' + AAttributeName + ' attribute from ' + ANode.nodeName);
    end;
  end;
end;

procedure TEnvOptions.ProcessProjectExtensions;
begin
  FindNode(FXmlDoc.DOMDocument.childNodes, 'ProjectExtensions',
    procedure(ANode: IDOMNode; var ACancel: boolean)
    var
      LValue: string;
    begin
      if HasAttribute(ANode.attributes, 'Condition', LValue) then
      begin
        Log('Found ProjectExtensions with Condition: ' + LValue);
        RemoveAttributeFromNode(ANode, 'Condition');
      end;
    end);
end;

function TEnvOptions.FindLibrary(AName: string): TEnvOptionNode;
begin
  Result := FEnvOptionNodes.FindLibrary(AName, false);
end;

procedure TEnvOptions.FindNode(
  ANodeList: IDOMNodeList;
  AName: string;
  AOnFound: TNodeFoundProc);
var
  LIdx: integer;
  LCancel: boolean;
  LNode: IDOMNode;
  LNodeName: string;
begin
  if Assigned(ANodeList) then
  begin
    LIdx := 0;
    LCancel := false;
    while (LIdx < ANodeList.length) and (not LCancel) do
    begin
      LNode := ANodeList[LIdx];
      LNodeName := LNode.nodeName;
      if SameText(LNodeName, AName) then
      begin
        if Assigned(AOnFound) then
          AOnFound(LNode, LCancel);
      end;
      if ANodeList[LIdx].hasChildNodes then
      begin
        FindNode(ANodeList[LIdx].childNodes, AName, AOnFound);
      end;
      Inc(LIdx);
    end;
  end;
end;

procedure TEnvOptions.LoadLibraryPaths(AEnvOptionNode: TEnvOptionNode);
begin
  if Assigned(AEnvOptionNode.PropertyGroup) then
  begin
    FindNode(AEnvOptionNode.PropertyGroup.childNodes, 'DelphiLibraryPath',
      procedure(ANode: IDOMNode; var ACancel: boolean)
      begin
        if ANode.hasChildNodes then
        begin
          AEnvOptionNode.DelphiLibraryPathNode := ANode.firstChild;
        end;
      end);
    FindNode(AEnvOptionNode.PropertyGroup.childNodes, 'DelphiBrowsingPath',
      procedure(ANode: IDOMNode; var ACancel: boolean)
      begin
        if ANode.hasChildNodes then
        begin
          AEnvOptionNode.DelphiBrowsingPathNode := ANode.firstChild;
        end;
      end);
    FindNode(AEnvOptionNode.PropertyGroup.childNodes, 'DelphiDebugDCUPath',
      procedure(ANode: IDOMNode; var ACancel: boolean)
      begin
        if ANode.hasChildNodes then
        begin
          AEnvOptionNode.DelphiDebugDCUPathNode := ANode.firstChild;
        end;
      end);
  end;
end;

procedure TEnvOptions.ClearEnvOptionNodes;
begin
  FEnvOptionNodes.Clear;
end;

procedure TEnvOptions.AddEnvOptionNodes;
begin
  ClearEnvOptionNodes;
  FEnvOptionNodes.Add(TEnvOptionNode.Create('Win32'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('Win64'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('Android'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('Android64'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('iOSDevice64'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('OSX64'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('OSXARM64'));
  FEnvOptionNodes.Add(TEnvOptionNode.Create('Linux64'));
end;

procedure TEnvOptions.Load;
begin
  Log('Loading: ' + EnvOptionsFileName);
  FXmlDoc := nil;

  FXmlDoc := TXMLDocument.Create(nil);
  FXmlDoc.LoadFromFile(EnvOptionsFileName);

  // Process ProjectExtensions nodes to remove Condition attributes
  // (A bug from JVCL installer https://issuetracker.delphi-jedi.org/view.php?id=6780)
  ProcessProjectExtensions;

  FindNode(FXmlDoc.DOMDocument.childNodes, 'PropertyGroup',
    procedure(ANode: IDOMNode; var ACancel: boolean)
    var
      LValue: string;
      LEnvOptionNode: TEnvOptionNode;
    begin
      if HasAttribute(ANode.attributes, 'Condition', LValue) then
      begin
        LEnvOptionNode := FEnvOptionNodes.FindLibrary(LValue, true);
        if Assigned(LEnvOptionNode) then
        begin
          LEnvOptionNode.PropertyGroup := ANode;
          LoadLibraryPaths(LEnvOptionNode);
          Log(LEnvOptionNode.AsString);
        end;
      end;
    end);
end;

procedure TEnvOptions.Save;
begin
  Log('Saving: ' + EnvOptionsFileName);
  if Assigned(FXmlDoc) and (not FXmlDoc.IsEmptyDoc) then
  begin
    if BackupCurrentConfig then
    begin
      FXmlDoc.SaveToFile(EnvOptionsFileName);
      Log('Saved: ' + EnvOptionsFileName);
    end
    else
    begin
      raise EEvnOptionException.Create('Failed to backup current config');
    end;
  end
  else
  begin
    raise EEvnOptionException.Create('EnvOptions has not been loaded');
  end;
end;

procedure TEnvOptions.ApplyLibraryPaths(
  ALibraryName: string;
  ALibraryPaths: TLibraryPaths);
var
  LNode: TEnvOptionNode;
begin
  LNode := FindLibrary(ALibraryName);
  if Assigned(LNode) then
  begin
    Log('ApplyLibraryPaths Before: ' + LNode.AsString);
    LNode.ApplyLibraryPaths(ALibraryPaths);
    Log('ApplyLibraryPaths After: ' + LNode.AsString);
  end;
end;

function TEnvOptions.AsXML: string;
begin
  Result := '';
  if Assigned(FXmlDoc) and (not FXmlDoc.IsEmptyDoc) then
  begin
    FXmlDoc.SaveToXML(Result);
  end;
end;

{ TEnvOptionNode }

procedure TEnvOptionNode.ApplyLibraryPaths(ALibraryPaths: TLibraryPaths);
begin
  DelphiBrowsingPath := ALibraryPaths.AsDelimitedString(dlpBrowse);
  DelphiDebugDCUPath := ALibraryPaths.AsDelimitedString(dlpDebugDCU);
  DelphiLibraryPath := ALibraryPaths.AsDelimitedString(dlpSearch);
end;

function TEnvOptionNode.AsString: string;
begin
  Result := FLibraryName + ' ';
  if Assigned(FDelphiBrowsingPathNode) then
    Result := Result + 'DelphiBrowsingPath: ' +
      FDelphiBrowsingPathNode.nodeValue + #13#10;
  if Assigned(FDelphiLibraryPathNode) then
    Result := Result + 'DelphiLibraryPath: ' +
      FDelphiLibraryPathNode.nodeValue + #13#10;
  if Assigned(FDelphiDebugDCUPathNode) then
    Result := Result + 'DelphiDebugDCUPath: ' +
      FDelphiDebugDCUPathNode.nodeValue + #13#10;
end;

constructor TEnvOptionNode.Create(ALibraryName: string);
begin
  FLibraryName := ALibraryName;
end;

function TEnvOptionNode.GetDelphiBrowsingPath: string;
begin
  Result := '';
  if Assigned(FDelphiBrowsingPathNode) then
    Result := FDelphiBrowsingPathNode.nodeValue;
end;

function TEnvOptionNode.GetDelphiDebugDCUPath: string;
begin
  Result := '';
  if Assigned(FDelphiDebugDCUPathNode) then
    Result := FDelphiDebugDCUPathNode.nodeValue;
end;

function TEnvOptionNode.GetDelphiLibraryPath: string;
begin
  Result := '';
  if Assigned(FDelphiLibraryPathNode) then
    Result := FDelphiLibraryPathNode.nodeValue;
end;

procedure TEnvOptionNode.SetDelphiBrowsingPath(const Value: string);
begin
  if Assigned(FDelphiBrowsingPathNode) then
    FDelphiBrowsingPathNode.nodeValue := Value;
end;

procedure TEnvOptionNode.SetDelphiBrowsingPathNode(const Value: IDOMNode);
begin
  FDelphiBrowsingPathNode := Value;
end;

procedure TEnvOptionNode.SetDelphiDebugDCUPath(const Value: string);
begin
  if Assigned(FDelphiDebugDCUPathNode) then
    FDelphiDebugDCUPathNode.nodeValue := Value;
end;

procedure TEnvOptionNode.SetDelphiDebugDCUPathNode(const Value: IDOMNode);
begin
  FDelphiDebugDCUPathNode := Value;
end;

procedure TEnvOptionNode.SetDelphiLibraryPath(const Value: string);
begin
  if Assigned(FDelphiLibraryPathNode) then
    FDelphiLibraryPathNode.nodeValue := Value;
end;

procedure TEnvOptionNode.SetDelphiLibraryPathNode(const Value: IDOMNode);
begin
  FDelphiLibraryPathNode := Value;
end;

procedure TEnvOptionNode.SetLibraryName(AName: string);
begin
  FLibraryName := AName;
end;

procedure TEnvOptionNode.SetPropertyGroupNode(const Value: IDOMNode);
begin
  FPropertyGroupNode := Value;
end;

{ TEnvOptionNodes }

function TEnvOptionNodes.FindLibrary(
  AName: string;
  AConditionSearch: boolean): TEnvOptionNode;
var
  LIdx: integer;
  LLibraryName: string;
begin
  Result := nil;
  LIdx := 0;
  while (LIdx < Count) and (not Assigned(Result)) do
  begin
    LLibraryName := Items[LIdx].LibraryName;
    if AConditionSearch then
    begin
      LLibraryName := '''$(Platform)''==''' + LLibraryName + '''';
    end;
    if SameText(LLibraryName, AName) then
    begin
      Result := Items[LIdx];
    end;
    Inc(LIdx);
  end;
end;

end.
