-module(tinypdf_file_util).

-export([pread/3]).


pread(Bin, eof, Bytes)
  when is_binary(Bin) ->
    binary:part(Bin, byte_size(Bin), -Bytes);
pread(File, eof, Bytes)
  when is_pid(File) ->
    {ok, Data} = file:pread(File, {eof, -Bytes}, Bytes),
    Data;
pread(Bin, Loc, _Bytes)
  when is_binary(Bin), Loc >= byte_size(Bin) ->
    eof;
pread(Bin, Loc, Bytes)
  when is_binary(Bin), Loc + Bytes > byte_size(Bin) ->
    binary:part(Bin, Loc, byte_size(Bin)-Loc);
pread(Bin, Loc, Bytes)
  when is_binary(Bin) ->
    binary:part(Bin, Loc, Bytes);
pread(File, Loc, Bytes)
  when is_pid(File) ->
    {ok, Data} = file:pread(File, Loc, Bytes),
    Data.
