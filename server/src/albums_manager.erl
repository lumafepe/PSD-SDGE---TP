-module(albums_manager).

-export([start/0, createAlbum/2,getAlbum/1]).

start() ->
    Albums = #{}, % {AlbumNome : PID } }
    register(?MODULE, spawn(fun() -> loop(Albums) end)).


rpc(Request) ->
    ?MODULE ! {Request,self()},
    receive
        {?MODULE, Result} -> Result
    end.

createAlbum(Name,Username) ->
    rpc({create,Name,Username}).

getAlbum(Name) ->
    rpc({get,Name}).

loop(Albums) ->
    receive
        {{create,Name,Username}, From} ->
            case maps:find(Name,Albums) of
                error ->
                    PID = spawn(fun() -> album_manager:start(Name,Username) end),
                    From ! {?MODULE, ok},
                    _Albums = maps:put(Name,PID,Albums),
                    loop(_Albums);
                _ ->
                    From ! {?MODULE, {error, "Album already exists"}},
                    loop(Albums)
            end;
        {{get,Name}, From} ->
            case maps:find(Name,Albums) of
                error ->
                    From ! {?MODULE, {error, "Album does't exists"}},
                    loop(Albums);
                {ok,PID} ->
                    From ! {?MODULE, {ok, PID}},
                    loop(Albums)
            end
    end.