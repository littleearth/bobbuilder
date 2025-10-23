{ -----------------------------------------------------------------------------
  Unit Name:Bob.StackTrace
  Author: Tristan Marlow
  Purpose: Enables an exception stack trace handler. Currently JclDebug is
  used.

  ----------------------------------------------------------------------------
  Copyright (c) 2016 Tristan David Marlow
  Copyright (c) 2016 Little Earth Solutions
  All Rights Reserved

  This product is protected by copyright and distributed under
  licenses restricting copying, distribution and decompilation

  ----------------------------------------------------------------------------

  History: 23/02/2016 - First Release.

  ----------------------------------------------------------------------------- }
unit Bob.StackTrace;

interface

{$DEFINE HAS_EXCEPTION_STACKTRACE}

uses
  SysUtils, Classes;


implementation

uses
  JclDebug, Lazy.Exception.Details,
  {$IFNDEF CONSOLE}
  Lazy.ExceptionDialog, VCL.Forms,
  {$ENDIF}
  Lazy.Exception.Details.Handler.JclDebug;

initialization

// Enable JCL stack tracking
JclDebug.JclStackTrackingOptions := [stStack, stRawMode];
  
// Register the JclDebug handler globally
TLZExceptionDetails.RegisterHandler(TLZJclExceptionHandler.Create);

{$IFDEF CONSOLE}
// For console applications, JclDebug stack tracking is initialized in Bob.Exception.Details
// Exception details will be logged via LazyLog when caught
{$ELSE}
  TLZExceptionDialog.SetApplicationExceptionHandler;
{$ENDIF}

end.
