unit Uutils;

{-------------------------------------------------------------------}
{                    Unit:    Uutils.pas                            }
{                    Project: EPANET2W                              }
{                    Version: 2.2                                   }
{                    Date:    6/24/19                               }
{                    Author:  L. Rossman                            }
{                                                                   }
{   Delphi Pascal unit containing general purpose utility           }
{   routines for EPANET2W.                                          }
{-------------------------------------------------------------------}

interface

uses SysUtils, Graphics, Windows, Math, Classes, Forms,
     Consts, Dialogs, System.UITypes, VCLTee.Chart;

const
  MAXSIZE     = 536870910; { = largest Integer / SizeOf(Integer) - 1 }

type
{ This is used by the CompareStrVals function }
  TRelationType   = (rtBelow, rtEquals, rtAbove);

{ These are used for dynamically allocated arrays.}
  TByteArray    = Array[0..MAXSIZE] of Byte;
  PByteArray    = ^TByteArray;
  TIntegerArray = Array[0..MAXSIZE] of Integer;
  PIntegerArray = ^TIntegerArray;
  TLongintArray = Array[0..MAXSIZE] of Longint;
  PLongintArray = ^TLongintArray;
  TSingleArray  = Array[0..MAXSIZE] of Single;
  PSingleArray  = ^TSingleArray;

  EInvalidDest = class(EStreamError);
  EFCantMove = class(EStreamError);

procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);

procedure ChartToPicture(aChart: TChart; aPicture: TPicture);
procedure Cls(Canvas: TCanvas; aRect: TRect; aColor: TColor);
function  CompareSingles(a, b: Pointer): Integer;
function  CompareStrVals(const S1,S2: String; const R: TRelationType): Boolean;

procedure CopyStringArray(const Source: array of String;
                          var Dest: array of String);
procedure CopyStringList(List1, List2: TStrings);
procedure DrawTextCentered(C: TCanvas; R: TRect; const S: String);
function  FileDateTime(const FileName: string): TDateTime;
function  FindKeyWord(const S: String; const Words: array of PChar;
            const N: Integer): Integer;
procedure FitChartToPage(aChart: TChart; const PageWidth, PageHeight: Single;
            var ChartWidth, ChartHeight: Single);

function  GetAppDataDir(const AppName: String; const AppDir: String): String;
function  GetDecimalChar:Char;
function  GetExtended(const S: String; var X: Extended): Boolean;
function  GetFileSize(const FileName: string): LongInt;
function  GetLocaleMeasurements: Integer;
function  GetSingle(const S: String; var X: Single): Boolean;
function  GetTempFile(const Folder, Prefix: String): String;
function  GetTempFolder: String;
function  GetTimeString(const Seconds: Longint): String;
function  GetWindowsDir: String;
function  HasAttr(const FileName: string; Attr: Word): Boolean;
function  InRect(const X: Integer; const Y: Integer;
                 const aRect: TRect): Boolean;
function  InvertColor(C: TColor): TColor;
function  IsValidNumber(const Txt: String; var V: Single): Boolean;
procedure LatLongToMeters(var X, Y: Extended);

function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;

function  PtOnLine(const P1: TPoint; const P2: TPoint;
                   const P: TPoint; const Ptol: Integer): Boolean;
function  StrHoursToFloat(S: String): Single;
procedure Swap(var x: Integer; var y: Integer);
procedure TextOutRotate(Canvas: TCanvas; X, Y: Integer; S: String;
                        fAngle: Longint);
procedure Tokenize(S: String; T: TStringlist; var N: Integer);
function  WinExecAndWait(CmdLine: String; ShowCmd: Integer): Integer;

implementation

uses
  ShellAPI;

const
  SInvalidDest = 'Destination %s does not exist';
  SFCantMove = 'Cannot move file %s';
  SInvalidNum = ' is not a valid number.';


procedure AutoScale(var Zmin: Double; var Zmax: Double; var T: Double);
//-----------------------------------------------------------------------------
//  Scales the range between Zmin and Zmax at intervals of T.
//-----------------------------------------------------------------------------
var
  m        : Integer;
  z        : Longint;
  d, z1, z2: Double;
begin
  z1 := Zmin;
  z2 := Zmax;
  try
    d := Abs(Zmax-Zmin);
    if (d = 0.0) and (Zmin = 0.0) then
    begin
      Zmin := -1.0;
      Zmax := 1.0;
      T := 1.0;
      Exit;
    end
    else if d < 0.01 then
    begin
      Zmin := Zmin - 0.5*Abs(Zmin);
      Zmax := Zmax + 0.5*Abs(Zmax);
    end;
    d := Abs(Zmax - Zmin);
    m := Trunc(Ln(d)/Ln(10.0));
    T := IntPower(10.,m);
    if T > 0.5*d then T := 0.2*T
    else if T > 0.2*d then T := 0.5*T;
    z := Trunc(Zmax/T) + 1;
    Zmax := z*T;
    z := Trunc(Zmin/T);
    if Zmin < 0 then z := z - 1;
    Zmin := z*T;
    if Zmin = Zmax then Zmax := Zmin + T;
    if Abs(Zmin-Zmax)/T > 10.0 then T := 2.0*T;
  except
    Zmin := z1;
    Zmax := z2;
    T := z2 - z1;
  end;
end;


procedure ChartToPicture(aChart: TChart; aPicture: TPicture);
//-----------------------------------------------------------
//  Converts a TeeChart chart object into a TPicture object.
//-----------------------------------------------------------
var
  mf: TMetafile;
  mfCanvas: TMetafileCanvas;
begin
  mf := TMetafile.Create;
  try
    mf.Width := aChart.Width;
    mf.Height := aChart.Height;
    mfCanvas := TMetafileCanvas.Create(mf,0);
      try
        aChart.PrintPartialCanvas(mfCanvas,Rect(0,0,aChart.Width,aChart.Height));
      finally
        mfCanvas.Free;
      end;
    aPicture.Graphic := mf;
  finally
    mf.Free;
  end;
end;


procedure Cls(Canvas: TCanvas; aRect: TRect; aColor: TColor);
{-----------------------------------------------------------}
{  Fills rectangle aRect with color aColor on Canvas.       }
{-----------------------------------------------------------}
var
  oldbrushstyle : TBrushStyle;
  oldbrushcolor : TColor;
begin
  with Canvas do
  begin
    oldbrushcolor := Brush.Color;
    oldbrushstyle := Brush.Style;
    Brush.Color := aColor;
    Brush.Style := bsSolid;
    FillRect(aRect);
    Brush.Style := oldbrushstyle;
    Brush.Color := oldbrushcolor;
  end;
end;


function CompareSingles(a, b: Pointer): Integer;
{-------------------------------------------------------------}
{  Compares values of pointers to two variables of type Single}
{-------------------------------------------------------------}
begin
  if (Single(a^) < Single(b^)) then Result := -1
  else if (Single(a^) > Single(b^)) then Result := 1
  else Result := 0;
end;


function CompareStrVals(const S1,S2: String; const R: TRelationType): Boolean;
{-----------------------------------------------------------------------}
{  Compares numerical values contained in strings S1 & S2 with          }
{  respect to relational operator R. If neither S1 or S2 are            }
{  numbers then a case-sensitive string comparison is made.             }
{-----------------------------------------------------------------------}
var
  v1, v2: Single;
  c: Integer;
begin
  Result := False;
  if (Uutils.GetSingle(S1,v1) and Uutils.GetSingle(S2,v2)) then
  begin
    case R of
      rtBelow: if (v1 >= v2) then exit;
      rtEquals: if (v1 <> v2) then exit;
      rtAbove: if (v1 <= v2) then exit;
    end;
    Result := True;
  end
  else
  begin
    c := CompareStr(S1, S2);
    case R of
      rtBelow:  if (c < 0) then Result := True;
      rtEquals: if (c = 0) then Result := True;
      rtAbove:  if (c > 0) then Result := True;
    end;
  end;
end;


procedure CopyStringArray(const Source: array of String;
  var Dest: array of String);
{-------------------------------------------------}
{  Copies array of strings Source to array Dest   }
{-------------------------------------------------}
var
  i: Integer;
  first, last: Integer;
begin
  first := MaxIntValue([Low(Source),Low(Dest)]);
  last :=  MinIntValue([High(Source),High(Dest)]);
  for i := first to last do Dest[i] := Source[i];
end;


procedure CopyStringList(List1, List2: TStrings);
{------------------------------------------------}
{  Copies StringList List1 to List2              }
{------------------------------------------------}
var
  P : pChar;
begin
  P := List1.GetText;
  List2.SetText(P);
  StrDispose(P);
end;


procedure DrawTextCentered(C: TCanvas; R: TRect; const S: String);
{----------------------------------------------------------------}
{ Draws text S centered in rectangle R on canvas C.              }
{----------------------------------------------------------------}
var
  left, top: Integer;
begin
  with C do
  begin
    top := R.Top + (R.Bottom - R.Top - TextExtent(S).cy) div 2;
    left := R.Left + (R.Right - R.Left - TextExtent(S).cx) div 2;
    TextOut(left, top, S);
  end;
end;

function FileDateTime(const FileName: string): System.TDateTime;
begin
  FileAge(FileName, Result);
end;


function FindKeyWord(const S: String; const Words: array of PChar;
  const N: Integer): Integer;
{-----------------------------------------------------------------}
{  Sees if the first N characters of any of the keywords in the   }
{  array Words matches the string S. Returns index of keyword if  }
{  match found or -1 if not found.                                }
{-----------------------------------------------------------------}
var
  k : Integer;
  s1, s2: String;
begin
  s2 := UpperCase(S);
  for k := 0 to High(Words) do
  begin
    s1 := Copy(Words[k], 1, N);
    if Pos(s1, s2) = 1 then
    begin
      Result := k;
      Exit;
    end;
  end;
  Result := -1;
end;


procedure FitChartToPage(aChart: TChart; const PageWidth, PageHeight: Single;
  var ChartWidth, ChartHeight: Single);
{------------------------------------------------------------}
{  Fits chart aChart as displayed on screen to printed page. }
{  PageWidth and PageHeight are the width and height of the  }
{  printable area of the page and ChartWidth and ChartHeight }
{  are returned as the width and height of the chart on the  }
{  printed page.                                             }
{------------------------------------------------------------}

var
  aratio,h,w: Single;
begin
  h := aChart.Height/Screen.PixelsPerInch;
  if aChart.Height > 0 then
    aratio := aChart.Width/aChart.Height
  else
    aratio := 1.0;
  w := h*aratio;
  if w > PageWidth then
  begin
    w := PageWidth;
    h := w/aratio;
  end;
  if h > PageHeight then
  begin
    h := PageHeight;
    w := h*aratio;
  end;
  ChartWidth := w;
  ChartHeight := h;
end;


function  GetAppDataDir(const AppName: String; const AppDir: String): String;
//-----------------------------------------------------------------------------
//  Returns a directory in a user's space where data files for
//  an application named AppName can be safely saved.
//-----------------------------------------------------------------------------
var
  S: String;
begin
  // Determine system's Application Data directory
  S := GetEnvironmentVariable('APPDATA');

  // If it doesn't exist, then get the user's home directory
  if Length(S) = 0 then S := GetEnvironmentVariable('USERPROFILE');

  // If we found the app or home directory, then create an AppName
  // directory under it
  if Length(S) > 0 then
  begin
    S := S + '\' + AppName;
    if not DirectoryExists(S) then
      if not CreateDir(S) then S := '';
  end;

  // If we still don't have a directory, then use the directory
  // where the application is installed
  if Length(S) = 0 then S := AppDir;

  // Append a backslash to the directory name
  Result := S + '\';
end;


function GetDecimalChar:Char;
{------------------------------------------------}
{  Retrieves International decimal point setting }
{------------------------------------------------}
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(2);
  GetLocaleInfo(GetSystemDefaultLCID,LOCALE_SDECIMAL,Buffer,2);
  Result := Buffer[0];
  StrDispose(Buffer);
end;


function  GetExtended(const S: String; var X: Extended): Boolean;
{-----------------------------------------------------------}
{  Converts string S to extended float X. Returns True if   }
{  S is a valid number and False if not.                    }
{-----------------------------------------------------------}
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;


function GetFileSize(const FileName: string): LongInt;
{----------------------------------------------------------------}
{  Returns the size of the named file without opening the file.  }
{  If the file doesn't exist, returns -1.                        }
{----------------------------------------------------------------}
var
  SearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
    Result := SearchRec.Size
  else Result := -1;
end;


function GetLocaleMeasurements: Integer;
{-----------------------------------------------}
{  Gets units of measurement for user's system  }
{  0 = Metric (Millimeters), 1 = US (inches)    }
{-----------------------------------------------}
var
  Buffer: PChar;
begin
  Buffer := StrAlloc(2);
  GetLocaleInfo(GetUserDefaultLCID, LOCALE_IMEASURE, Buffer, 2);
  if Buffer[0] = '0' then
    Result := 0
  else
    Result := 1;
  StrDispose(Buffer);
end;


function  GetSingle(const S: String; var X: Single): Boolean;
{--------------------------------------------------------}
{  Converts string S to Single float X. Returns True if  }
{  S is a valid number and False if not.                 }
{--------------------------------------------------------}
begin
  X := 0;
  Result := True;
  try
    X := StrToFloat(S);
  except
    On EConvertError do Result := False;
  end;
end;


function GetTempFile(const Folder, Prefix: String): String;
{----------------------------------------------------------}
{  Returns a unique filename guaranteed not to be in use.  }
{  Folder = name of folder where file will be created      }
{  (use '.' for current folder). Prefix = string whose     }
{  first 3 characters form beginning of the filename.      }
{----------------------------------------------------------}
var
  zFileName: array[0..MAX_PATH] of char;
  zFolder: array[0..MAX_PATH] of char;
  zPrefix: array[0..3] of char;
begin
  FillChar(zFileName, SizeOf(zFileName), #0);
  StrPLCopy(zFolder, Folder, MAX_PATH);
  StrPLCopy(zPrefix, Prefix, 3);
  if GetTempFileName(zFolder, zPrefix, 0, zFilename) <> 0 then
    Result := zFileName
  else
    Result := '';
end;


function GetTempFolder: String;
{---------------------------------------------------}
{  Returns file path designated for temporary files.}
{---------------------------------------------------}
var
  zTmpFolder: array[0..MAX_PATH] of char;
begin
  FillChar(zTmpFolder, SizeOf(zTmpFolder), #0);
  if GetTempPath(MAX_PATH,zTmpFolder) <> 0 then
    Result := zTmpFolder
  else
    Result := '';
end;


function GetTimeString(const Seconds: Longint): String;
{----------------------------------------------}
{  Converts seconds into Hrs:Mins format       }
{----------------------------------------------}
var
  hours, minutes: Longint;
begin
  hours := Seconds div 3600;
  minutes := (Seconds - (3600*hours)) div 60;
  Result := Format('%d:%.2d ',[hours,minutes]);
end;


function GetWindowsDir: String;
{----------------------------------------------}
{  Returns Windows directory with '\' appended.}
{----------------------------------------------}
var
  WinDir  : array[0..MAX_PATH] of Char;
begin
  if (GetWindowsDirectory(WinDir,MAX_PATH) > 0) then
  begin
    if WinDir[StrLen(WinDir)-1] <> '\' then StrCat(WinDir,'\');
    Result := String(WinDir);
  end
  else Result := '';
end;


function HasAttr(const FileName: string; Attr: Word): Boolean;
begin
  //Result := (FileGetAttr(FileName) and Attr) = Attr;
  Result := (FileGetAttr(FileName) and Attr) > 0;
end;


function InRect(const X: Integer; const Y: Integer;
                const aRect: TRect): Boolean;
{-------------------------------------------------------}
{  Determines if point X,Y falls within rectangle aRect.}
{-------------------------------------------------------}
begin
  with aRect do
  begin
    if (X >= Left) and (X <= Right)
    and (Y >= Top) and (Y <= Bottom)
    then Result := True
    else Result := False;
  end;
end;

function  InvertColor(C: TColor): TColor;
{----------------------------------------}
{  Returns inverse of color C            }
{----------------------------------------}
var
  rgb1, rgb2: LongInt;
  r,g,b: Byte;
begin
  rgb1 := ColorToRGB(C);
  r := GetRValue(rgb1) mod 255;
  g := GetGValue(rgb1) mod 255;
  b := GetBValue(rgb1) mod 255;
  rgb2 := RGB(r,g,b);
  if rgb2 = rgb1 then rgb2 := 0;
  Result := TColor(rgb2);
end;

function IsValidNumber(const Txt: String; var V: Single): Boolean;
{-------------------------------------------}
{  Checks if string Txt is a valid number.  }
{-------------------------------------------}
begin
    if not GetSingle(Txt,V) then
    begin
      MsgDlg('''' + Txt + '''' + SInvalidNum, mtError,[mbOK]);
      Result := False;
    end
    else Result := True;
end;


procedure LatLongToMeters(var X, Y: Extended);
{--------------------------------------------}
{ Converts longitude (X) and latitude (Y) to }
{ Cartesian coordinates in meters. Assumes a }
{ spherical earth with radius of 6,370,997 m }
{ so 1 degree = 111194.87 m at the equator.  }
{--------------------------------------------}
begin
  X := 111194.87*X;
  Y := 111194.87*Y*cos(DegToRad(Y));
end;


function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons): Integer; overload;
//-----------------------------------------------------------------------------
//  Displays a message dialog in center of currently active form.
//-----------------------------------------------------------------------------
begin
  Result :=  MsgDlg(Msg, DlgType, Buttons, Screen.ActiveForm);
end;


function  MsgDlg(const Msg: string; DlgType: TMsgDlgType;
          Buttons: TMsgDlgButtons; F: TForm): Integer; overload;
//-----------------------------------------------------------------------------
//  Displays a message dialog in center of a specific form.
//-----------------------------------------------------------------------------
var
  R: TRect;
begin
  if not Assigned(F) then
  begin
    Result := MessageDlg(Msg, DlgType, Buttons, 0);
  end else
  begin
    with CreateMessageDialog(Msg, DlgType, Buttons) do
    try
      GetWindowRect(F.Handle, R);
      Left := R.Left + ((R.Right - R.Left) div 2) - (Width div 2);
      Top := R.Top + ((R.Bottom - R.Top) div 2) - (Height div 2);
      Result := ShowModal;
    finally
      Free;
    end;
  end;
end;


function PtOnLine(const P1: TPoint; const P2: TPoint;
  const P: TPoint; const Ptol: Integer): Boolean;
{-------------------------------------------------------}
{  Checks if point P is on line between points P1 & P2  }
{-------------------------------------------------------}
var
  dx,  dy  : Integer;
  dx1, dy1 : Integer;
  a, b, c  : Integer;
begin
  Result := False;
  dx := P2.X - P1.X;
  dy := P2.Y - P1.Y;
  dx1 := P.X - P1.X;
  dy1 := P.Y - P1.Y;
  if (Abs(dx) > 0) and (Abs(dy) < Abs(dx)) then
  begin
    if (dx*dx1 >= 0) and (Abs(dx1) <= Abs(dx)) then
    begin
      a := (dy*dx1);
      b := (dx*dy1);
      c := Abs(dx*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end
  else if Abs(dy) > 0 then
  begin
    if (dy*dy1 >= 0) and (Abs(dy1) <= Abs(dy)) then
    begin
      a := (dx*dy1);
      b := (dy*dx1);
      c := Abs(dy*Ptol);
      if Abs(a-b) <= c then Result := True;
    end;
  end;
end;


function  StrHoursToFloat(S: String): Single;
{-----------------------------------------------------}
{  Converts time in Hours:Mins:Secs to decimal hours  }
{-----------------------------------------------------}
var
  n: Integer;
  hr: Integer;
  min: Integer;
  sec: Integer;
begin
  hr := 0;
  min := 0;
  sec := 0;
  try

  // If no ':' separator then string is a decimal number
    S := Trim(S);
    n := Pos(':',S);
    if (n = 0) then
    begin
      if not GetSingle(S,Result) then Result := -1;
    end

  // Parse string into hours, minutes, & seconds
    else
    begin
      if (n > 1) then hr := StrToInt(Copy(S,1,n-1));
      Delete(S,1,n);
      n := Pos(':',S);
      if (n = 0) then min := StrToInt(S)
      else
      begin
        if (n > 1) then min := StrToInt(Copy(S,1,n-1));
        Delete(S,1,n);
        if (Length(S) > 0) then sec := StrToInt(S);
      end;
      Result := hr + (min)/60 + (sec)/3600;
    end;
  except
    on EConvertError do Result := -1;
  end;
end;


procedure Swap(var X: Integer; var Y: Integer);
{-----------------------------------------------}
{  Swaps the values of integers X & Y.          }
{-----------------------------------------------}
var
  Z : Integer;
begin
  Z := X;
  X := Y;
  Y := Z;
end;


procedure TextOutRotate(Canvas: TCanvas; X, Y: Integer; S: String;
  fAngle: Longint);
{------------------------------------------------------------------}
{  Draws string S at location X,Y on Canvas rotated fAngle degrees.}
{------------------------------------------------------------------}
var
  LogRec            : TLOGFONT;
  OldFont, NewFont  : HFONT;
  H, W              : Integer;
  fDegToRad         : Double;
  fCosAngle         : Double;
  fSinAngle         : Double;
begin
  with Canvas do
  begin
    fDegToRad := PI/180.0;
    fCosAngle := cos(fAngle*fDegToRad);
    fSinAngle := sin(fAngle*fDegToRad);
    {Brush.Style := bsClear;}
    GetObject(Font.Handle, SizeOf(LogRec), @LogRec);
    LogRec.lfEscapement := fAngle*10;
    LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFont := CreateFontIndirect(LogRec);
    OldFont := SelectObject(Canvas.Handle,NewFont);
    W := TextWidth(S);
    H := TextHeight(S);
    X := X - trunc(W/2*fCosAngle) - trunc(H/2*fSinAngle);
    Y := Y + trunc(W/2*fSinAngle) - trunc(H/2*fCosAngle);
    TextOut(X, Y, S);
    NewFont := SelectObject(Canvas.Handle,OldFont);
    DeleteObject(NewFont);
  end;
end;


procedure Tokenize(S: String; T: TStringList; var N: Integer);
{----------------------------------------------------}
{  Parses string S into N tokens stored in T.        }
{  Words between " " are stored as a single token.   }
{  Characters to right of ';' are ignored.           }
{----------------------------------------------------}
const
// Spaces, tabs, commas, & line feeds separate tokens
//  separators: set of Char = [' ',Chr(9),',',Chr(10),Chr(13)];
  separators: TSysCharSet = [' ',Chr(9),',',Chr(10),Chr(13)];
var
  instring:  Boolean;   {True if currently in a string}
  intoken:   Boolean;   {True if currently in a token}
  i:         Integer;   {Current position in S}
  start:     Integer;   {Start position of item}
  len:       Integer;   {Length of item}
  c:         Char;      {Current character in S}
begin
// Initialize variables
  T.Clear;
  N := 0;
  intoken := False;
  instring := False;
  start := 1;
  len := 0;

// Examine each character in S
  i := 1;
  while (i <= Length(S)) do
  begin
    c := S[i];
    if c = ';' then break;     {Comment follows}

//    if  (c in separators)
    if CharInSet(c, separators)
    and (not instring) then    {Separator found}
    begin
      if intoken then          {Finish current token}
      begin
        T.Add(Copy(S,start,len));
        intoken := False;
        Inc(N);
      end;
    end

    else if (c = Chr(34))      {Quote found}
    and not intoken then
    begin
      if instring then         {Finish current string}
      begin
        T.Add(Copy(S,start,len));
        instring := False;
        Inc(N);
      end

      else                     {Start new string}
      begin
        instring := True;
        start := i+1;
        len := 0;
      end;
    end

    else                       {Neither separator nor quote}
    begin
      if (not intoken)         {If no current token/string}
      and (not instring) then  {then start new token}
      begin
        intoken := True;
        start := i;
        len := 1;
      end
      else Inc(len);           {Otherwise add to current one}
    end;

    Inc(i);                    {Move to next character}
  end;

  if (intoken)
  or (instring) then           {Finish current item}
  begin
    T.Add(Copy(S,start,len));
    Inc(N);
  end;
end;


function WinExecAndWait(CmdLine: String; ShowCmd: Integer): Integer;
{---------------------------------------------------------}
{  Executes program given by CmdLine.                     }
{  Program's window state given by ShowCmd.               }
{---------------------------------------------------------}
var
  zCmdLine: array[0..512] of char;
  zCurDir: array[0..255] of char;
  WorkDir: String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  ExitCode: DWORD;
begin
  StrPCopy(zCmdLine, CmdLine);
  GetDir(0,WorkDir);
  StrPCopy(zCurDir,WorkDir);
  FillChar(StartupInfo,SizeOf(StartupInfo),#0);
  StartupInfo.cb := Sizeof(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := ShowCmd;
  if not CreateProcess(nil,
    zCmdLine,
    nil,
    nil,
    False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS,
    nil,
    zCurDir,
    StartupInfo,
    ProcessInfo) then Result := -1
  else
  begin
    WaitForSingleObject(ProcessInfo.hProcess,INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess,ExitCode);
    Result := ExitCode;
  end;
end;
end.
