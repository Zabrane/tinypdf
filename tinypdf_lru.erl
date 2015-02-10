-module(tinypdf_lru).

-export([new/1, find/2, insert/3]).


new(Capacity) ->
    Table = ets:new(lru_cache, [set, private]),
    {lru, Capacity, 0, 0, Table}.


find(Key, {lru, Capacity, Size, Serial, Table}) ->
    case ets:update_element(Table, Key, [{3, Serial}]) of
        false ->
            not_found;
        true ->
            {ets:lookup_element(Table, Key, 2),
             {lru, Capacity, Size, Serial+1, Table}}
    end.


insert(Key, Value, {lru, Capacity, Size, Serial, Table}) ->
    true = ets:insert_new(Table, {Key, Value, Serial}),
    remove_old_entries({lru, Capacity, Size+1, Serial+1, Table}).


remove_old_entries({lru, Capacity, Size, Serial, Table})
  when Size > Capacity ->
    OldSerial =
        lists:min(ets:select(Table, [{{'_','_','$1'},[],['$1']}])),
    Deleted = ets:select_delete(Table, [{{'_','_',OldSerial},[],[true]}]),
    {lru, Capacity, Size-Deleted, Serial, Table};
remove_old_entries(LRU) ->
    LRU.
