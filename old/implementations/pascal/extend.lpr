// ********************************************************
//  Copyright (c) 2016 Rob Judd <judd@ob-wan.com>
//  Based on C version by Charles Childers et al
//  ISC License - see included file LICENSE
// ********************************************************

program extend;

{$mode objfpc}{$H+}
{$macro on}

//{$define VERBOSE}

uses
  SysUtils, bridge in 'bridge.pas', nga in 'nga.pas';

{$include 'nga.inc'}

//implementation

function include_file(fname : PChar) : Integer;
var
  tokens : Integer = 0;
  source : array[0..63999] of Char;
  handle : THandle;
  f : File of Char;
 begin
  handle := FileOpen(fname, fmOpenRead);
  if handle = THandle(-1) then
  begin
    writeln(format('Unable to open %s!', [fname]));
    halt();
  end;
  FileClose(handle);
  AssignFile(f, fname);
  Reset(f, SizeOf(Char));
  try
    while not eof(f) do
    begin
      read_token(f, source);
{$ifdef VERBOSE}
      writeln(format('compiling ___ %s ___', [source]));
{$endif}
      evaluate(source);
      inc(tokens);
    end;
  finally
    CloseFile(f);
  end;
  result := tokens;
end;

procedure stats();
begin
  update_rx();
  writeln(format('  Heap @ %d', [Heap]));
end;

// ********************************************************
//  Main program
// ********************************************************
var
  tokens : Integer;
  handle : THandle;
begin
  writeln('RETRO12');
  writeln('+ initialize');
  ngaPrepare();
  writeln('+ load image');
  ngaLoadImage('ngaImage');
  stats();
  writeln(format('+ load %s', [paramStr(1)]));
  tokens := include_file(PChar(paramStr(1)));
  writeln(format('  processed %d tokens', [tokens]));
  stats();
  writeln('+ save new image');

  handle := FileOpen('ngaImage', fmOpenWrite);
  if handle = THandle(-1) then
  begin
    writeln('Unable to save the ngaImage!');
    exit();
  end;
  FileWrite(handle, memory, sizeof(Cell) * (memory[3] + 1));
  FileClose(handle);
end.
