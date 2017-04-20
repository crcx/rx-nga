// ********************************************************
//  Copyright (c) 2016 Rob Judd <judd@ob-wan.com>
//  Based on C version by Charles Childers et al
//  ISC License - see included file LICENSE
// ********************************************************

program editor;

{$mode objfpc}{$H+}
{$macro on}

uses
  SysUtils, bridge in 'bridge.pas', nga in 'nga.pas', vt100 in 'vt100.pas';

const
  ED_BUFFER = 327680;
  ED_BLOCKS = 384;

{$include 'nga.inc'}

var
  Current, Column, Row, Mode : Cell;

//implementation

procedure update_state();
begin
  update_rx();
  Current := memory[d_xt_for('ed:Current', Dictionary)];
  Column := memory[d_xt_for('ed:Col', Dictionary)];
  Row := memory[d_xt_for('ed:Row', Dictionary)];
  Mode := memory[d_xt_for('ed:Mode', Dictionary)];
end;

procedure sep();
var
  i : Integer;
begin
  for i := 0 to 7 do
    write('--------');
  writeln();
end;

procedure rho(block, n : Integer);
var
  start, i : Integer;
begin
  start := (block * 512) + (n * 64);
  for i := 0 to 63 do
    write(Char(memory[ED_BUFFER + start + i] and $FF));
  writeln();
end;

procedure stats();
var
  M : Char;
begin
  if mode = 1 then
    M := 'I'
  else
    M := 'C';
  write(format('Free: %d | Heap: %d | ', [326140 - Heap, Heap]));
  writeln(format('Cur %d : Row %d : Col %d | Mode %s', [Current, Row, Column, M]));
end;

procedure block_display(n : Integer);
var
  line : Integer;
begin
  for line := 0 to 7 do
    rho(n, line);
  sep();
  update_rx();
  stats();
end;

procedure red_enter(ch : Integer);
begin
  stack_push(ch);
  evaluate('ed:insert-char');
end;

procedure display_stack();
var
  i : Cell;
begin
  for i := 1 to sp do
  begin
    if i = sp then
      write(format('< %d >', [data[i]]))
    else
      write(format('%d ', [data[i]]));
  end;
  writeln();
end;

procedure save();
var
  handle : THandle;
begin
  memory[d_xt_for('ed:Mode', Dictionary)] := 0;
  handle := FileCreate('ngaImage+editor', fmOpenWrite);
  if handle = THandle(-1) then
  begin
    writeln('Unable to save the ngaImage!');
    halt();
  end;
  FileWrite(handle, memory, SizeOf(Cell) * IMAGE_SIZE);
  FileClose(handle);
end;

procedure write_buffer();
var
  handle : THandle;
  slot : Cell;
  i : Integer;
begin
  handle := FileCreate('retro.blocks', fmOpenWrite);
  if handle <> THandle(-1) then
  begin
    for i := ED_BUFFER to IMAGE_SIZE - 1 do
    begin
      slot := memory[i];
      FileWrite(handle, slot, sizeof(Cell));
    end;
    FileClose(handle);
  end;
end;

procedure read_blocks();
var
  handle : THandle;
  slot : Cell;
  i : Integer;
begin
  handle := FileOpen('retro.blocks', fmOpenRead);
  if handle <> THandle(-1) then
  begin
    for i := ED_BUFFER to IMAGE_SIZE - 1 do
    begin
      FileRead(handle, slot, sizeof(Cell));
      memory[i] := slot;
    end;
    FileClose(handle);
  end;
end;

procedure initialize_rx();
begin
  ngaPrepare();
  ngaLoadImage('ngaImage+editor');
  read_blocks();
  update_state();
end;

// ********************************************************
//  Main program
// ********************************************************
var
  ch : Integer;
  c : array[0..6] of Char;
  i : array[0..6] of Char;
  dt : Cell;
begin
  initialize_rx();
  term_setup();
  c := 'ed:c_?';
  i := 'ed:i_?';
  while true do
  begin
    update_state();
    term_clear();
    block_display(Current);
    display_stack();
    term_move_cursor(Column + 1, Row + 1);
    read(ch);
    if Mode = 0 then
    begin
      c[5] := Char(ch);
      dt := d_lookup(Dictionary, c);
      if dt <> 0 then
        execute(memory[d_xt(dt)]);
    end
    else if Mode = 1 then
    begin
      i[5] := Char(ch);
      dt := d_lookup(Dictionary, i);
      if dt <> 0 then
        execute(memory[d_xt(dt)])
      else
        red_enter(ch);
    end;
    update_state();
    if Mode = 2 then
    begin
      term_move_cursor(1, 15);
      term_cleanup();
      save();
      write_buffer();
      break;
    end;
  end;
end.

