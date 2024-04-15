-module(album_manager).

-export([start/0, listAlbums/1, addUser/1, createAlbum/2]).

start() ->
    Albums = #{},
    Accesses = #{
        "MAX" => sets:new(),
        "LEWIS" => sets:new(),
        "SERGIO" =>  sets:new()
        },
    register(?MODULE, spawn(fun() -> loop(Albums,Accesses) end)).

rpc(Request) ->
    ?MODULE ! {Request,self()},
    receive
        {?MODULE, Result} -> Result
    end.

listAlbums(Username) ->
    rpc({listAlbums,Username}).

addUser(Username) ->
    rpc({addUser,Username}).

createAlbum(Username,Name) ->
    rpc({create,Username,Name}).



addAlbumName(Albums, Name) ->
    maps:put(Name,sets:new(),Albums).

addAlbumAccess(Accesses, Name, Username) ->
    maps:update(Username,sets:add_element(Name,maps:find(Username,Accesses)) ,Accesses).


%Accesses = Map{Username => Set{Albums}}
%Albums = Map{Name => Set{files}}
loop(Albums,Accesses) ->
    receive
        {{addUser,Username}, From} ->
            From ! {?MODULE, ok},
            loop(Albums,maps:put(Username,sets:new(), Accesses));

        {{listAlbums, Username}, From} ->
            Albums = map:find(Username,Accesses),
            From ! {?MODULE, {ok, Albums}},
            loop(Albums,createAlbumAccesses);

        {{create,Username,Name}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, ok},
                    loop(addAlbumName(Albums,Name),addAlbumAccess(Accesses, Name, Username));
                _ ->
                    From ! {?MODULE, {error, "Album already exists"}},
                    loop(Albums,Accesses)
            end
    end.

    
