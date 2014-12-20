-module(tinypdf_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs =
        [
         { tinypdf_file_server,
           {tinypdf_file_server, start_link, []},
           permanent, 2000, worker,
           [tinypdf_file_server]
         },
         { tinypdf_file_reader_sup,
           {tinypdf_file_reader_sup, start_link, []},
           permanent, 2000, worker,
           [tinypdf_file_reader_sup]
         }
        ],

    {ok, {SupFlags, ChildSpecs}}.
