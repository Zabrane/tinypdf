-module(tinypdf_lru).

-export([new/1, insert/3, find/2]).

-record(lru, {capacity, size, serial, key2value, key2serial, serial2key}).


new(Capacity) ->
    #lru {
       capacity = Capacity,
       size = 0,
       serial = 0,
       key2value = dict:new(),
       key2serial = dict:new(),
       serial2key = gb_trees:empty()
      }.


insert(Key, Value, LRU = #lru{size=Size, serial=Serial, key2value=Key2Value, key2serial=Key2Serial, serial2key=Serial2Key}) ->
    error = dict:find(Key, Key2Serial),
    resize(
        LRU #lru {
          size = Size + 1,
          serial = Serial + 1,
          key2value = dict:store(Key, Value, Key2Value),
          key2serial = dict:store(Key, Serial, Key2Serial),
          serial2key = gb_trees:insert(Serial, Key, Serial2Key)
         }).


resize(LRU = #lru{capacity=Capacity, size=Size})
  when Size =< Capacity ->
    LRU;
resize(LRU = #lru{size=Size, key2value=Key2Value, key2serial=Key2Serial, serial2key=Serial2Key}) ->
    {Serial, Key} = gb_trees:smallest(Serial2Key),
    LRU #lru {
      size = Size - 1,
      key2value = dict:erase(Key, Key2Value),
      key2serial = dict:erase(Key, Key2Serial),
      serial2key = gb_trees:delete(Serial, Serial2Key)
     }.


find(Key, LRU = #lru{serial=Serial, key2value=Key2Value, key2serial=Key2Serial, serial2key=Serial2Key}) ->
   case dict:find(Key, Key2Value) of
       {ok, Value} ->
           {ok, OldSerial} = dict:find(Key, Key2Serial),

           { ok,
             Value,
             LRU #lru {
               serial = Serial + 1,
               key2serial = dict:store(Key, Serial, Key2Serial),
               serial2key = gb_trees:insert(Serial, Key, gb_trees:delete(OldSerial, Serial2Key))
              }};
       error ->
           error
   end.
