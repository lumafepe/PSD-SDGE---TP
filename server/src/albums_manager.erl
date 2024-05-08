-module(albums_manager).

-export([start/0, listAlbums/1, addUser/1, createAlbum/2,editAlbum/4,leave/6]).

start() ->
    Albums = #{}, % {AlbumNome : {PID,{Username : status} } }
    AccessesByPerson = #{},  % {Username : Set(AlbumNome) }
    register(?MODULE, spawn(fun() -> loop(Albums,AccessesByPerson) end)).


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

editAlbum(Username,Name,Adress,Port) ->
    rpc({edit,Username,Name,Adress,Port}).


leave(Username,Name,Clock,Position,Users,Files) ->
    rpc({leave,Username,Name,Clock,Position,Users,Files}).

    

getOnline(Users) ->
    maps:filter(
        fun (Username, State) ->
            case State of
                false ->
                    false;
                _ ->
                    true
            end
        end,Users).



getUsersRemoved(Old,New) ->
    sets:subtract(Old,New).

getUsersAdded(Old,New) ->
    sets:subtract(New,Old).

addAlbumToUsers(AlbumName,Users,AccessesByPerson) ->
    sets:fold(
        fun(U,Acc) ->
            {ok, CurrAlbums} = maps:find(U,Acc),
            maps:update(U,sets:add_element(AlbumName,CurrAlbums),Acc) 
        end,
    AccessesByPerson,Users).
removeAlbumFromUsers(AlbumName,Users,AccessesByPerson) ->
    sets:fold(
        fun(U,Acc) ->
            {ok, CurrAlbums} = maps:find(U,Acc),
            maps:update(U,sets:del_element(AlbumName,CurrAlbums),Acc) 
        end,
    AccessesByPerson,Users).

addUsersToAlbum(Users,CurrData) ->
    sets:fold(
        fun(U,Acc) ->
            
            maps:put(U,false,Acc)
        end,
        CurrData,Users).
removeUsersFromAlbum(Users,CurrData) ->
    sets:fold(
        fun(U,Acc) ->
            maps:remove(U,Acc)
        end,
    CurrData,Users).



loop(Albums,AccessesByPerson) ->
    receive
        {{listAlbums, Username}, From} ->
            {ok , AlbumsList} =  maps:find(Username,AccessesByPerson),
            From ! {?MODULE, {ok , AlbumsList}},
            loop(Albums,AccessesByPerson);
        
        {{addUser,Username}, From} ->
            case maps:find(Username,AccessesByPerson) of
                error ->
                    From ! {?MODULE, ok},
                    _AccessesByPerson = maps:put(Username,sets:new(),AccessesByPerson),
                    loop(Albums,_AccessesByPerson);
                _ ->
                    From ! {?MODULE, {error, "Person already exists"}},
                    loop(Albums,AccessesByPerson)
            end;
        {{create,Username,Name}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, ok},
                    {ok, _CurrSet} = maps:find(Username,AccessesByPerson),
                    _AccessesByPerson = maps:update(Username,sets:add_element(Name,_CurrSet),AccessesByPerson),
                    PID = spawn(fun() -> album_manager:start() end),
                    _Albums = maps:put(Name,{PID,#{Username=>false}},Albums),
                    loop(_Albums,_AccessesByPerson);
                _ ->
                    From ! {?MODULE, {error, "Album already exists"}},
                    loop(Albums,AccessesByPerson)
            end;
        
        {{leave,Username,Name,Clock,Position,NewUsers,Files}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,Users}} ->
                    album_manager:writeAlbum(PID,Files), % escrever ficheiros
                    album_manager:addClock(PID,Clock,Position), % escrever o clock
                    UsersAdded = getUsersAdded(Users,sets:from_list(NewUsers)),
                    UsersRemoved = getUsersRemoved(Users,sets:from_list(NewUsers)),
                    _AccessesByPerson = addAlbumToUsers(Name,UsersAdded,AccessesByPerson),
                    __AccessesByPerson = removeAlbumFromUsers(Name,UsersRemoved,_AccessesByPerson),
                    _Users = addUsersToAlbum(UsersAdded,Users),
                    __Users = removeUsersFromAlbum(UsersRemoved,_Users),
                    case maps:find(Username,__Users) of
                        {ok, _} ->
                            _Albums = maps:update(Name,{PID,maps:update(Username,false,__Users)},Albums),
                            From ! {?MODULE, {ok}},
                            loop(_Albums,__AccessesByPerson);
                        true ->
                            _Albums = maps:update(Name,{PID,__Users},Albums),
                            From ! {?MODULE, {ok}},
                            loop(_Albums,__AccessesByPerson)
                    end
            end;

        {{edit,Username,Name,Adress,Port}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,Users}} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            _Users = maps:update(Username,{Adress,Port},Users),
                            _Albums = maps:update(Name,{PID,_Users},Albums),
                            _OnlineMap = getOnline(Users),
                            _Online = maps:values(_OnlineMap),
                            if
                                _Online == [] ->
                                    {ok,Files} = album_manager:getAlbum(PID),
                                    From ! {?MODULE, {files, Files,maps:keys(Users)}},
                                    loop(_Albums,AccessesByPerson);
                                true ->
                                    {Clock,Position} = album_manager:getClock(PID),
                                    From ! {?MODULE, {online, _OnlineMap,Clock,Position}},
                                    loop(_Albums,AccessesByPerson)
                            end;
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end
    end.