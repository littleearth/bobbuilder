unit Bob.MarkdownToRTF;

interface

uses Classes, Windows, SysUtils, Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg, Lazy.Utils, Lazy.Types, Bob.Common;

type
  TMarkdownToRTF = class(TLZObject)
  protected
    FInputFileName: string;
    FInTable: Boolean;
    function RTFEscape(const AText: string): string;
    function SimpleEscape(const AText: string): string;
    function RTFUnicode(const AText: string): string;
    function ExtractUrlFromLinkParam(const AParam: string): string;
    function ProcessMarkdownLine(const ALine: string; var AOutput: string; var AInCodeBlock: Boolean): Boolean;
    function ProcessHeading(const ALine: string; ALevel: Integer): string;
    function ProcessBold(const AText: string): string;
    function ProcessItalic(const AText: string): string;
    function ProcessCodeInline(const AText: string): string;
    function ProcessLink(const AText: string; const AUrl: string): string;
    function ProcessImage(const AAltText: string; const AUrl: string): string;
    function ProcessUrl(const AUrl: string): string;
    function ProcessList(const AText: string; ALevel: Integer): string;
  public
    function Convert(AInputFileName: TFileName; AOutputFileName: TFileName): Boolean;
  end;

  TMarkdownToRTFConsole = class(TMarkdownToRTF)
  protected
    function GetUsage: string;
    function CheckCommandParameter(var AMessage: string; var AInputFile: TFileName; var AOutputFile: TFileName): Boolean;
  public
    function Execute(var AMessage: string): Boolean;
  end;

implementation

uses
  System.RegularExpressions, System.Math;

function UTF8ToString(const AUTF8Text: AnsiString): string;
var
  LMultiByteLen: Integer;
begin
  LMultiByteLen := Length(AUTF8Text);
  if LMultiByteLen = 0 then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, LMultiByteLen);
  LMultiByteLen := MultiByteToWideChar(CP_UTF8, 0, PAnsiChar(AUTF8Text), Length(AUTF8Text), PChar(Result), Length(Result));
  SetLength(Result, LMultiByteLen);
end;

function TMarkdownToRTF.RTFEscape(const AText: string): string;
var
  i: Integer;
  LInRTFCode: Boolean;
begin
  Result := '';
  LInRTFCode := False;
  
  for i := 1 to Length(AText) do
  begin
    // Detect RTF codes (sequences starting with \ followed by letters)
    if (AText[i] = '\') and (i < Length(AText)) and CharInSet(AText[i + 1], ['a'..'z', 'A'..'Z', '''', '-']) then
    begin
      // This is the start of an RTF code - don't escape the backslash
      Result := Result + AText[i];
      LInRTFCode := True;
    end
    else if LInRTFCode and not CharInSet(AText[i], ['a'..'z', 'A'..'Z', '0'..'9', '''', '-']) then
    begin
      // End of RTF code - now process this character
      LInRTFCode := False;
      case AText[i] of
        '\': Result := Result + '\\';
        '{': Result := Result + '\{';
        '}': Result := Result + '\}';
        #13: Result := Result + '\par'#13#10' ';
        #10: ; // Ignore
      else
        Result := Result + AText[i];
      end;
    end
    else if LInRTFCode then
    begin
      // Still in RTF code
      Result := Result + AText[i];
    end
    else
    begin
      // Regular text - escape special characters
      case AText[i] of
        '\': Result := Result + '\\';
        '{': Result := Result + '\{';
        '}': Result := Result + '\}';
        #13: Result := Result + '\par'#13#10' ';
        #10: ; // Ignore, we handle line breaks explicitly
      else
        Result := Result + AText[i];
      end;
    end;
  end;
end;

function TMarkdownToRTF.SimpleEscape(const AText: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AText) do
  begin
    case AText[i] of
      '\': Result := Result + '\\';
      '{': Result := Result + '\{';
      '}': Result := Result + '\}';
      '"': Result := Result + '\"';
    else
      Result := Result + AText[i];
    end;
  end;
end;

function TMarkdownToRTF.RTFUnicode(const AText: string): string;
var
  i: Integer;
  LCode: Word;
begin
  Result := '';
  for i := 1 to Length(AText) do
  begin
    LCode := Ord(AText[i]);
    // RTF Unicode format: \uCodePoint? where ? is fallback character
    if LCode > 127 then
    begin
      // Unicode character - encode as \uNNNN?
      Result := Result + Format('\u%d?', [LCode]);
    end
    else
    begin
      // ASCII character - escape special RTF characters
      case AText[i] of
        '\': Result := Result + '\\';
        '{': Result := Result + '\{';
        '}': Result := Result + '\}';
      else
        Result := Result + AText[i];
      end;
    end;
  end;
end;

function TMarkdownToRTF.ExtractUrlFromLinkParam(const AParam: string): string;
var
  S: string;
  P: Integer;
begin
  // Trim and strip optional title or surrounding angle brackets
  S := Trim(AParam);
  // If contains a space, take only the URL portion before the space (title after)
  P := Pos(' ', S);
  if P > 0 then
    S := Copy(S, 1, P - 1);
  // Strip surrounding parentheses (if any from regex capture) and angle brackets
  if (Length(S) >= 2) and (S[1] = '<') and (S[Length(S)] = '>') then
    S := Copy(S, 2, Length(S) - 2);
  Result := S;
end;

function TMarkdownToRTF.ProcessCodeInline(const AText: string): string;
begin
  // Escape the code text, then wrap in font codes
  Result := RTFEscape(Trim(AText));
  Result := '\f1\fs22 ' + Result + '\f0\fs24 '; // Consolas font (f1), smaller size
end;

function TMarkdownToRTF.ProcessBold(const AText: string): string;
begin
  // Escape the text, then wrap in bold codes
  Result := RTFEscape(Trim(AText));
  Result := '\b ' + Result + '\b0 ';
end;

function TMarkdownToRTF.ProcessItalic(const AText: string): string;
begin
  // Escape the text, then wrap in italic codes
  Result := RTFEscape(Trim(AText));
  Result := '\i ' + Result + '\i0 ';
end;

function TMarkdownToRTF.ProcessHeading(const ALine: string; ALevel: Integer): string;
const
  SizeMap: array[1..6] of Integer = (36, 28, 22, 18, 16, 14);
begin
  Result := '\b\fs' + IntToStr(SizeMap[Min(ALevel, 6)]) + ' ' + ALine + '\b0\fs24\par'#13#10;
end;

function TMarkdownToRTF.ProcessList(const AText: string; ALevel: Integer): string;
begin
  Result := '\li' + IntToStr(ALevel * 360) + ' - ' + AText + '\par'#13#10;
end;

function TMarkdownToRTF.ProcessLink(const AText: string; const AUrl: string): string;
var
  LDispRaw: string;
  LDispRtf: string;
  LUrl: string;
begin
  // Use RTF field for clickable link. Display text is blue + underlined.
  LDispRaw := Trim(AText);

  // If the display is inline code (e.g., `code`), render with code formatting
  if (Length(LDispRaw) >= 2) and (LDispRaw[1] = '`') and (LDispRaw[Length(LDispRaw)] = '`') then
    LDispRtf := ProcessCodeInline(Copy(LDispRaw, 2, Length(LDispRaw) - 2))
  else if (Pos('\', LDispRaw) > 0) or (Pos('{', LDispRaw) > 0) or (Pos('}', LDispRaw) > 0) then
    // Assume already RTF-formatted (e.g., bold/italic codes applied)
    LDispRtf := LDispRaw
  else
    LDispRtf := SimpleEscape(LDispRaw);

  if LDispRtf = '' then
    LDispRtf := SimpleEscape(Trim(AUrl));

  LUrl := SimpleEscape(Trim(AUrl));
  // Emit standard RTF hyperlink: underline + blue for display; leading space prevents first char loss
  // Add trailing space after field to prevent loss of next character in some RTF readers
  Result := '{\field{\*\fldinst HYPERLINK "' + LUrl + '"}{\fldrslt{\ul\cf2 ' + LDispRtf + '}}} ';
end;

function TMarkdownToRTF.ProcessImage(const AAltText: string; const AUrl: string): string;
var
  LStream: TMemoryStream;
  LImageData: TBytes;
  LHexString: string;
  LPicture: TPicture;
  LWidth, LHeight: Integer;
  LMaxDisplayPoints, LTwipsPerPoint: Integer;
  LDisplayWidthTwips, LDisplayHeightTwips: Integer;
  LOriginalWidthTwips, LOriginalHeightTwips: Integer;
  i: Integer;
  LExtension: string;
  LFormat: string;
  LImagePath: string;
begin
  // Try to load and embed the image
  try
    // Determine the image path - relative to the input markdown file's directory
    LImagePath := Trim(AUrl);
    
    // Remove leading slash from relative paths
    if (Length(LImagePath) > 0) and (LImagePath[1] = '/') then
      LImagePath := Copy(LImagePath, 2, MaxInt);
    
    // Convert forward slashes to backslashes for Windows
    LImagePath := StringReplace(LImagePath, '/', '\', [rfReplaceAll]);
    
    // Make path relative to the markdown file's directory
    if ExtractFileDrive(LImagePath) = '' then
    begin
      if ExtractFilePath(FInputFileName) <> '' then
        LImagePath := IncludeTrailingPathDelimiter(ExtractFilePath(FInputFileName)) + LImagePath;
    end;
    
    // If the path doesn't exist, fall back to plain text
    if not FileExists(LImagePath) then
    begin
      if AAltText <> '' then
        Result := AAltText + ' (' + AUrl + ')'
      else
        Result := AUrl;
      Exit;
    end;
    
    // Determine format from extension
    LExtension := LowerCase(ExtractFileExt(LImagePath));
    
    // Only support PNG and JPEG for now
    if not ((LExtension = '.png') or (LExtension = '.jpg') or (LExtension = '.jpeg')) then
    begin
      if AAltText <> '' then
        Result := AAltText + ' (' + AUrl + ')'
      else
        Result := AUrl;
      Exit;
    end;
    
    // Read the image file as binary - we'll use dummy dimensions for now
    LWidth := 200;  // Default size
    LHeight := 200;
    
    // Try to get actual dimensions if Graphics is available
    try
      LPicture := TPicture.Create;
      try
        LPicture.LoadFromFile(LImagePath);
        LWidth := LPicture.Width;
        LHeight := LPicture.Height;
      finally
        LPicture.Free;
      end;
    except
      // Use default dimensions if we can't load the image for dimension info
    end;
    
    // Read the image file as binary
    LStream := TMemoryStream.Create;
    try
      LStream.LoadFromFile(LImagePath);
      SetLength(LImageData, LStream.Size);
      LStream.Position := 0;
      LStream.Read(LImageData[0], LStream.Size);
    finally
      LStream.Free;
    end;
    
    // Convert to hex string - limit to reasonable size to avoid huge RTF files
    if Length(LImageData) > 500000 then
    begin
      if AAltText <> '' then
        Result := AAltText + ' (' + AUrl + ')'
      else
        Result := AUrl;
      Exit;
    end;
    
    LHexString := '';
    for i := 0 to Length(LImageData) - 1 do
    begin
      LHexString := LHexString + IntToHex(LImageData[i], 2);
    end;
    
    // Determine the largest dimension and scale to a reasonable size
    // Target display width of 216 points (3 inches) to keep RTF files reasonable
    LMaxDisplayPoints := 216;  // 216 points = 216 * 20 twips = 4320 twips
    LTwipsPerPoint := 20;
    
    if LWidth > LHeight then
    begin
      // Landscape or square - use width as primary dimension
      LDisplayWidthTwips := LMaxDisplayPoints * LTwipsPerPoint;
      LDisplayHeightTwips := Round(LDisplayWidthTwips * LHeight / LWidth);
    end
    else
    begin
      // Portrait - use height as primary dimension
      LDisplayHeightTwips := LMaxDisplayPoints * LTwipsPerPoint;
      LDisplayWidthTwips := Round(LDisplayHeightTwips * LWidth / LHeight);
    end;
    
    // Original dimensions in twips (1 pixel = 15 twips at 96 DPI)
    LOriginalWidthTwips := LWidth * 15;
    LOriginalHeightTwips := LHeight * 15;
    
    // Determine format code
    if LExtension = '.png' then
      LFormat := 'pngblip'
    else
      LFormat := 'jpegblip';
    
    // Generate RTF picture control
    // Use original dimensions for picw/pich, display dimensions for picwgoal/pichgoal
    Result := '{\pict\' + LFormat + '\picw' + IntToStr(LOriginalWidthTwips) + '\pich' + IntToStr(LOriginalHeightTwips) + 
              '\picwgoal' + IntToStr(LDisplayWidthTwips) + '\pichgoal' + IntToStr(LDisplayHeightTwips) + ' ' + LHexString + '}';
  except
    // If anything fails, fall back to plain text
    if AAltText <> '' then
      Result := AAltText + ' (' + AUrl + ')'
    else
      Result := AUrl;
  end;
end;

function TMarkdownToRTF.ProcessUrl(const AUrl: string): string;
var
  LText: string;
  LUrl: string;
begin
  // Bare URL → clickable field with same text as display
  LText := SimpleEscape(Trim(AUrl));
  LUrl := LText;

  // Add leading and trailing spaces around field to avoid adjacent characters swallowing first display char
  Result := ' ' + '{\field{\*\fldinst HYPERLINK "' + LUrl + '"}{\fldrslt{\ul\cf2 ' + LText + '}}} ';
end;

function TMarkdownToRTF.ProcessMarkdownLine(const ALine: string; var AOutput: string; var AInCodeBlock: Boolean): Boolean;
var
  LTrimmedLine: string;
  LHeadingMatch: TMatch;
  LListMatch: TMatch;
  LProcessedLine: string;
  LIsTableLine: Boolean;
  LIsSeparator: Boolean;
  LCells: TStringList;
  LCellCount: Integer;
  LColWidthTwips: Integer;
  LColRight: Integer;
  i: Integer;
  LLineNoPipes: string;
  LIsHeaderRow: Boolean;

  // Helper procedure to process inline formatting
  procedure DoInlineFormatting(var Text: string);
  var
    TempCode, TempBold, TempItalic, TempLink, TempImage, TempUrl: TMatch;
    LField: string;
    LMatchedUrl: string;
  begin
    // Special-case: bold-wrapped markdown link **[text](url)**
    if Pos('{\field{', Text) = 0 then
    begin
      TempLink := TRegEx.Match(Text, '\*\*\[([^\]]+)\]\(([^\)]+)\)\*\*');
      while TempLink.Success do
      begin
        LField := ProcessLink(TempLink.Groups[1].Value, ExtractUrlFromLinkParam(TempLink.Groups[2].Value));
        // inject bold start after fldrslt{
        LField := StringReplace(LField, '{\fldrslt{\ul\cf2 ', '{\fldrslt{\b \ul\cf2 ', []);
        // inject bold end before fldrslt close
        LField := StringReplace(LField, '}}}', '\b0}}}', [rfReplaceAll]);
        Text := StringReplace(Text, TempLink.Value, LField, []);
        if Pos('{\field{', Text) > 0 then
          Break;
        TempLink := TempLink.NextMatch;
      end;
    end;

    // Special-case: italic-wrapped markdown link *[text](url)*
    if (Pos('{\field{', Text) = 0) then
    begin
      TempLink := TRegEx.Match(Text, '\*\[([^\]]+)\]\(([^\)]+)\)\*');
      while TempLink.Success do
      begin
        LField := ProcessLink(TempLink.Groups[1].Value, ExtractUrlFromLinkParam(TempLink.Groups[2].Value));
        // inject italic start
        LField := StringReplace(LField, '{\fldrslt{\ul\cf2 ', '{\fldrslt{\i \ul\cf2 ', []);
        // inject italic end before close
        LField := StringReplace(LField, '}}}', '\i0}}}', [rfReplaceAll]);
        Text := StringReplace(Text, TempLink.Value, LField, []);
        if Pos('{\field{', Text) > 0 then
          Break;
        TempLink := TempLink.NextMatch;
      end;
    end;

    // Handle images (![alt](url)) first - but only if the text doesn't already contain field codes
    if Pos('{\field{', Text) = 0 then
    begin
      TempImage := TRegEx.Match(Text, '!\[([^\]]*)\]\(([^\)]+)\)');
      while TempImage.Success do
      begin
        Text := StringReplace(Text, TempImage.Value, ProcessImage(TempImage.Groups[1].Value, TempImage.Groups[2].Value), []);
        TempImage := TempImage.NextMatch;
      end;
    end;
    
    // Handle links ([text](url) or [text](<url>) or with optional title) - if no field yet
    if Pos('{\field{', Text) = 0 then
    begin
      TempLink := TRegEx.Match(Text, '\[([^\]]+)\]\(([^\)]+)\)');
      while TempLink.Success do
      begin
        Text := StringReplace(Text, TempLink.Value,
          ProcessLink(TempLink.Groups[1].Value, ExtractUrlFromLinkParam(TempLink.Groups[2].Value)), []);
        if Pos('{\field{', Text) > 0 then
          Break;
        TempLink := TempLink.NextMatch;
      end;
    end;
    
    // If a field was created already (e.g., a markdown link), do NOT process bare URLs
    if Pos('{\field{', Text) = 0 then
    begin
      // Handle bare URLs (http:// or https://) or angle-bracket autolinks
      TempUrl := TRegEx.Match(Text, '(<https?://[^>]+>)|(https?://[^\s]+)');
      while (TempUrl.Success) and (Pos('{\field{', Text) = 0) do
      begin
        // Prefer explicit captured text checks; some engines mark groups as Success even if empty
        if (Length(TempUrl.Groups[1].Value) > 0) then
        begin
          // Angle-bracket form captured; prefer the captured group value
          LMatchedUrl := TempUrl.Groups[1].Value;
          // Strip angle brackets only if still present in group (defensive)
          if (Length(LMatchedUrl) >= 2) and (LMatchedUrl[1] = '<') and (LMatchedUrl[Length(LMatchedUrl)] = '>') then
            LMatchedUrl := Copy(LMatchedUrl, 2, Length(LMatchedUrl) - 2);
          Text := StringReplace(Text, TempUrl.Value, ProcessUrl(LMatchedUrl), []);
        end
        else
        begin
          // Plain URL form; use the plain URL capture if available
          if (Length(TempUrl.Groups[2].Value) > 0) then
            LMatchedUrl := TempUrl.Groups[2].Value
          else
            LMatchedUrl := TempUrl.Value;
          Text := StringReplace(Text, TempUrl.Value, ProcessUrl(LMatchedUrl), []);
        end;
        TempUrl := TempUrl.NextMatch;
      end;
    end;
    
    // Check if field codes were created (hyperlinks/images)
    // If so, skip further processing to avoid corrupting the RTF codes
    if Pos('{\field{', Text) > 0 then
      Exit;
    
    // Skip bold/italic/code processing if field codes are present
    if Pos('{\field{', Text) = 0 then
    begin
      // Handle inline code (`code`) to avoid conflicts; do this BEFORE bold/italic
      TempCode := TRegEx.Match(Text, '`([^`]+)`');
      while TempCode.Success do
      begin
        Text := StringReplace(Text, TempCode.Value, ProcessCodeInline(TempCode.Groups[1].Value), []);
        TempCode := TempCode.NextMatch;
      end;
      
      // Handle bold (**text**)
      TempBold := TRegEx.Match(Text, '\*\*([^*]+)\*\*');
      while TempBold.Success do
      begin
        Text := StringReplace(Text, TempBold.Value, ProcessBold(TempBold.Groups[1].Value), []);
        TempBold := TempBold.NextMatch;
      end;
      
      // Handle italic (*text*)
      TempItalic := TRegEx.Match(Text, '\*([^*]+)\*');
      while TempItalic.Success do
      begin
        Text := StringReplace(Text, TempItalic.Value, ProcessItalic(TempItalic.Groups[1].Value), []);
        TempItalic := TempItalic.NextMatch;
      end;
    end;
  end;

begin
  Result := True;
  LTrimmedLine := Trim(ALine);
  // Detect simple Markdown table lines
  // Heuristic: lines with pipes like | a | b |, or separator lines of - : and |
  LIsTableLine := (Pos('|', ALine) > 0) and (
    TRegEx.IsMatch(ALine, '^\s*\|.*\|\s*$') or
    TRegEx.IsMatch(ALine, '^\s*[^\n]*\|[^\n]*$')
  );
  // Detect separator row (---, :---:, etc.) optionally bounded by pipes
  LIsSeparator := TRegEx.IsMatch(Trim(ALine), '^\|?\s*[:\-]+\s*(\|\s*[:\-]+\s*)+\|?$');

  // Handle code blocks (```)
  if LTrimmedLine.StartsWith('```') then
  begin
    if AInCodeBlock then
    begin
      // End code block - reset to normal font
      AOutput := AOutput + '\par}'#13#10;
      AInCodeBlock := False;
    end
    else
    begin
      // Start code block - use Consolas font
      AOutput := AOutput + '{\f1\fs22'#13#10;
      AInCodeBlock := True;
    end;
    Exit;
  end;

  // Markdown table row: render as RTF table
  if LIsTableLine then
  begin
    // Skip pure separator lines
    if LIsSeparator then
    begin
      // Remain in table mode
      Exit;
    end;

    // Determine if this is the first row in a contiguous table block
    LIsHeaderRow := not FInTable;
    FInTable := True;

    // Split into cells
    LLineNoPipes := Trim(ALine);
    if (Length(LLineNoPipes) > 0) and (LLineNoPipes[1] = '|') then
      LLineNoPipes := Trim(Copy(LLineNoPipes, 2, MaxInt));
    if (Length(LLineNoPipes) > 0) and (LLineNoPipes[Length(LLineNoPipes)] = '|') then
      LLineNoPipes := Trim(Copy(LLineNoPipes, 1, Length(LLineNoPipes) - 1));

    LCells := TStringList.Create;
    try
      LCells.StrictDelimiter := True;
      LCells.Delimiter := '|';
      LCells.DelimitedText := LLineNoPipes;
      // Trim spaces around each cell
      for i := 0 to LCells.Count - 1 do
        LCells[i] := Trim(LCells[i]);

      LCellCount := LCells.Count;
      if LCellCount = 0 then
      begin
        // Fallback to plain text
        AOutput := AOutput + RTFEscape(ALine) + '\par'#13#10;
        Exit;
      end;

      // Simple equal-width columns: 3000 twips (~2.1 in) each
      LColWidthTwips := 3000;

      // Table row header with borders
      AOutput := AOutput + '\trowd\trgaph108';
      LColRight := 0;
      for i := 0 to LCellCount - 1 do
      begin
        LColRight := LColRight + LColWidthTwips;
        // Per-cell borders: top, left, bottom, right
        AOutput := AOutput +
          '\clbrdrt\brdrs\brdrw10' +
          '\clbrdrl\brdrs\brdrw10' +
          '\clbrdrb\brdrs\brdrw10' +
          '\clbrdrr\brdrs\brdrw10' +
          '\cellx' + IntToStr(LColRight);
      end;

      // Cells
      for i := 0 to LCellCount - 1 do
      begin
        if LIsHeaderRow then
          AOutput := AOutput + '\intbl \f1\b ' + RTFEscape(LCells[i]) + ' \b0\f0\cell'
        else
          AOutput := AOutput + '\intbl \f1 ' + RTFEscape(LCells[i]) + ' \f0\cell';
      end;

      // End row and reset paragraph props
      AOutput := AOutput + '\row\pard'#13#10;
    finally
      LCells.Free;
    end;
    Exit;
  end;

  if AInCodeBlock then
  begin
    // Inside code block - use RTF Unicode encoding
    AOutput := AOutput + RTFUnicode(ALine) + '\par'#13#10;
    Exit;
  end;

  LProcessedLine := ALine;

  // Handle headings (#)
  LHeadingMatch := TRegEx.Match(LTrimmedLine, '^(#{1,6})\s+(.+)$');
  if LHeadingMatch.Success then
  begin
    LProcessedLine := LHeadingMatch.Groups[2].Value;
    DoInlineFormatting(LProcessedLine);
    AOutput := AOutput + ProcessHeading(LProcessedLine, LHeadingMatch.Groups[1].Length);
    Exit;
  end;

  // Handle bullet lists (-)
  LListMatch := TRegEx.Match(LProcessedLine, '^(\s*)-(.+)$');
  if LListMatch.Success then
  begin
    LProcessedLine := LListMatch.Groups[2].Value;
    DoInlineFormatting(LProcessedLine);
    AOutput := AOutput + ProcessList(LProcessedLine, LListMatch.Groups[1].Length div 2);
    Exit;
  end;

  // Handle regular lines with inline formatting
  DoInlineFormatting(LProcessedLine);
  
  // Regular line - Handle escaping based on content
  if not LProcessedLine.Trim.IsEmpty then
  begin
    // Check if line contains RTF formatting codes
    // If it does, output as-is. Otherwise, escape special characters.
    if (Pos('{\field{', LProcessedLine) > 0) or (Pos('\pict\', LProcessedLine) > 0) or (Pos('\b', LProcessedLine) > 0) or (Pos('\i', LProcessedLine) > 0) or (Pos('\f1', LProcessedLine) > 0) or (Pos('\cf', LProcessedLine) > 0) or (Pos('\ul', LProcessedLine) > 0) then
    begin
      // Line contains RTF codes - output directly
      AOutput := AOutput + LProcessedLine + '\par'#13#10;
    end
    else
    begin
      // Plain text - escape special RTF characters
      AOutput := AOutput + RTFEscape(LProcessedLine) + '\par'#13#10;
    end;
  end
  else
    AOutput := AOutput + '\par'#13#10;

  // If we were in a table and this line is not a table line, end the table block
  if FInTable and (not LIsTableLine) then
  begin
    FInTable := False;
  end;
end;

function TMarkdownToRTF.Convert(AInputFileName: TFileName; AOutputFileName: TFileName): Boolean;
var
  LInput: TStringList;
  LOutput: TStringList;
  i: Integer;
  LRTF: string;
  LInCodeBlock: Boolean;
  LFileStream: TFileStream;
  LUTF8Bytes: TBytes;
  LUTF8Text: AnsiString;
begin
  Result := False;
  LInput := TStringList.Create;
  LOutput := TStringList.Create;
  LInCodeBlock := False;
  FInputFileName := AInputFileName;
  FInTable := False;
  try
    if not FileExists(AInputFileName) then
      Exit;

    if FileExists(AOutputFileName) then
    begin
      TLZFile.Delete (AOutputFileName);
    end;

    // Load file as UTF-8 and convert to Unicode string
    LFileStream := TFileStream.Create(AInputFileName, fmOpenRead or fmShareDenyWrite);
    try
      SetLength(LUTF8Bytes, LFileStream.Size);
      LFileStream.Read(LUTF8Bytes[0], LFileStream.Size);
    finally
      LFileStream.Free;
    end;
    
    SetString(LUTF8Text, PAnsiChar(LUTF8Bytes), Length(LUTF8Bytes));
    LInput.Text := UTF8ToString(LUTF8Text);

    // RTF header with Unicode support
    LRTF := '{\rtf1\ansi\ansicpg1252\deff0'#13#10;
    LRTF := LRTF + '{\fonttbl{\f0\fswiss\fcharset0 Aptos;}{\f1\fmodern\fcharset0 Consolas;}{\f2\fswiss\fcharset0 Arial;}{\f3\ftech\fcharset0 Symbol;}}'#13#10;
    LRTF := LRTF + '{\colortbl;\red0\green0\blue0;\red0\green0\blue255;}'#13#10;
    LRTF := LRTF + '\viewkind4\uc1\pard\f0\fs24'#13#10;

    // Process each line
    for i := 0 to LInput.Count - 1 do
    begin
      ProcessMarkdownLine(LInput[i], LRTF, LInCodeBlock);
    end;

    // RTF footer
    LRTF := LRTF + '}';

    // Save output
    LOutput.Text := LRTF;
    LOutput.SaveToFile(AOutputFileName);
    Result := FileExists(AOutputFileName);
  finally
    LInput.Free;
    LOutput.Free;
  end;
end;

{ TMarkdownToRTFConsole }

function TMarkdownToRTFConsole.CheckCommandParameter(var AMessage: string; var AInputFile: TFileName; var AOutputFile: TFileName): Boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';

  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/INPUT', LValue) then
    begin
      Result := False;
      AMessage := GetUsage;
    end
    else
    begin
      AInputFile := LValue;
      if not FileExists(AInputFile) then
      begin
        Result := False;
        AMessage := Format('Input file not found: %s', [AInputFile]);
      end;
    end;
  end;

  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/OUTPUT', LValue) then
    begin
      Result := False;
      AMessage := GetUsage;
    end
    else
    begin
      AOutputFile := LValue;
    end;
  end;
end;

function TMarkdownToRTFConsole.Execute(var AMessage: string): Boolean;
var
  LInputFile: TFileName;
  LOutputFile: TFileName;
begin
  Result := False;
  if CheckCommandParameter(AMessage, LInputFile, LOutputFile) then
  begin
    Result := Convert(LInputFile, LOutputFile);
    if Result then
      AMessage := Format('Successfully converted %s to %s', [LInputFile, LOutputFile])
    else
      AMessage := Format('Failed to convert %s', [LInputFile]);
  end;
end;

function TMarkdownToRTFConsole.GetUsage: string;
var
  LUsage: TStringList;
begin
  LUsage := TStringList.Create;
  try
    LUsage.Add('bobmdtortf - Convert Markdown files to RTF format');
    LUsage.Add('');
    LUsage.Add('NOTE: %ERRORLEVEL% will be set on failures');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add('/INPUT:{filename} /OUTPUT:{filename}');
    LUsage.Add('');
    LUsage.Add('- /INPUT  - Input Markdown file (.md)');
    LUsage.Add('- /OUTPUT - Output RTF file (.rtf)');
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /INPUT:"README.md" /OUTPUT:"README.rtf"');
    LUsage.Add('   /INPUT:"C:\docs\readme.md" /OUTPUT:"C:\docs\readme.rtf"');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

end.

