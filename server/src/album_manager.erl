-module(album_manager).

-export([start/0, listAlbums/1, addUser/1, createAlbum/2, getAlbum/2]).

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

getAlbum(Username,Name) ->
    rpc({get,Username,Name}).


addAlbumName(Albums, Name) ->
    maps:put(Name,sets:new(),Albums).

addAlbumAccess(Accesses, Name, Username) ->
    case maps:find(Username,Accesses) of
        {ok, CurrAlbums} ->
            NewAlbums = sets:add_element(Name,CurrAlbums),
            maps:update(Username,NewAlbums,Accesses);
        _ -> % Will never happen sinse it is verified that the user has access to the Album before
            Accesses
end.


%Accesses = Map{Username => Set{Albums}}
%Albums = Map{Name => Set{files}}
loop(Albums,Accesses) ->
    receive
        {{addUser,Username}, From} ->
            From ! {?MODULE, ok},
            loop(Albums,maps:put(Username,sets:new(), Accesses));

        {{listAlbums, Username}, From} ->
            case maps:find(Username,Accesses) of
                {ok , AlbumsList} ->
                    From ! {?MODULE, {ok, AlbumsList}},
                    loop(Albums,Accesses);
                _ -> % will never happen
                    From ! {?MODULE, {ok, sets:new()}},
                    loop(Albums,Accesses)
            end;
        {{get,Username,Name}, From} ->
            case maps:find(Name,Albums) of
                {ok, Album} ->
                    case maps:find(Username,Accesses) of
                        {ok, AlbumsList} -> 
                            case sets:is_element(Name,AlbumsList) of
                                true ->
                                    From ! {?MODULE, Album},
                                    loop(Albums,Accesses);
                                false ->
                                    From ! {?MODULE, {error, "User doesn't have permission"}},
                                    loop(Albums,Accesses)
                            end;
                        _ -> %Will never happen
                            loop(Albums,Accesses)
                        end;
                _ ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,Accesses)
            end;

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

    
