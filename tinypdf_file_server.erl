-module(tinypdf_file_server).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([open/1, close/1, controlling_process/2]).

-define(SERVER, ?MODULE).

-record(state, {file2pid, pid2files}).


open(Filename) ->
    gen_server:call(?SERVER, {open, Filename}).

close(File) ->
    gen_server:call(?SERVER, {close, File}).

controlling_process(File, Pid) ->
    gen_server:call(?SERVER, {controlling_process, File, Pid}).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{file2pid=dict:new(), pid2files=dict:new()}}.


handle_call({open, Filename}, {From, _}, State) ->
    case file:open(Filename, [read, binary]) of
        {ok, File} ->
            true = link(From),
            {reply, {ok, File}, add_file(File, From, State)};
        Result ->
            {reply, Result, State}
    end;
handle_call({close, File}, _From, State = #state{file2pid=File2Pid}) ->
    case dict:find(File, File2Pid) of
        {ok, Pid} ->
            case file:close(File) of
                ok ->
                    unlink(Pid),
                    {reply, ok, remove_file(File, State)};
                Result ->
                    {reply, Result, State}
            end;
        error ->
            {reply, {error, badarg}, State}
    end;
handle_call({controlling_process, File, Pid}, {From, _}, State = #state{file2pid=File2Pid}) ->
    case dict:find(File, File2Pid) of
        {ok, From} ->
            true = link(Pid),
            true = unlink(From),
            {reply,
             ok,
             add_file(File, Pid, remove_file(File, State))};
        _ ->
            {reply, {error, not_owner}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, badrequest}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _}, State = #state{file2pid=File2Pid, pid2files=Pid2Files}) ->
    case dict:find(Pid, Pid2Files) of
        {ok, Files} ->
            [file:close(File) || File <- Files],
            {noreply, 
             #state{
                file2pid=lists:foldl(fun dict:erase/2, File2Pid, Files),
                pid2files=dict:erase(Pid, Pid2Files)}};
        error ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{file2pid=File2Pid}) ->
    Files = dict:fetch_keys(File2Pid),
    [file:close(File) || File <- Files],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_file(File, Pid, #state{file2pid=File2Pid, pid2files=Pid2Files}) ->
    #state {
       file2pid=dict:store(File, Pid, File2Pid),
       pid2files=dict:append(Pid, File, Pid2Files)}.

remove_file(File, #state{file2pid=File2Pid, pid2files=Pid2Files}) ->
    Pid = dict:fetch(File, File2Pid),
    Files = dict:fetch(Pid, Pid2Files),
    #state{
       file2pid = dict:erase(File, File2Pid),
       pid2files = dict:store(Pid, lists:delete(File, Files), Pid2Files)}.
