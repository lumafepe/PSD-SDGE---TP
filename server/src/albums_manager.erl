-module(albums_manager).

-export([start/0, listAlbums/1, addUser/1, createAlbum/2,editAlbum/4,isLast/2,leave/4,update/4]).

start() ->
    Albums = #{}, % {AlbumNome : {PID,inUse,{Username : status} } }
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

isLast(Username,Name) ->
    rpc({isLast,Username,Name}).

leave(Username,Name,Clock,Position) ->
    rpc({leave,Username,Name,Clock,Position}).

update(Username,Name,Files,Users) ->
    rpc({update,Username,Name,Files,Users}).
    

getOnline(Users) ->
    Status = maps:values(Users),
    lists:filter(
        fun(State)-> 
            case State of
                false ->
                    false;
                _ ->
                    true
            end
        end, Status).
    


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
                    _Albums = maps:put(Name,{PID,false,#{Username=>false}},Albums),
                    loop(_Albums,_AccessesByPerson);
                _ ->
                    From ! {?MODULE, {error, "Album already exists"}},
                    loop(Albums,AccessesByPerson)
            end;
        
        {{isLast,Username,Name}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,InUse,Users}} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            case InUse of
                                false ->
                                    _Online = getOnline(Users),
                                    _Albums = maps:update(Name,{PID,Username,maps:update(Username,false,Users)},Albums),
                                    if
                                        length(_Online) == 1 ->
                                            From ! {?MODULE, true},
                                            loop(_Albums,AccessesByPerson);
                                        true ->
                                            From ! {?MODULE, false},
                                            loop(_Albums,AccessesByPerson)
                                    end;
                                _ -> 
                                    From ! {?MODULE, {error, "Can't leave at the moment"}},
                                    loop(Albums,AccessesByPerson)
                            end;
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end;
        {{update,Username,Name,Files,NewUsers}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,InUse,Users}} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            album_manager:writeAlbum(PID,Files),
                            OldUsers = sets:from_list(maps:keys(Users)),
                            _NewUsers = sets:from_list(NewUsers),
                            _Users = maps:from_list(lists:map(fun(U)-> {U,false} end,NewUsers)),
                            UsersAdded = sets:subtract(_NewUsers,OldUsers),
                            UsersRemoved = sets:subtract(OldUsers,_NewUsers),
                            _AccessesByPerson = sets:fold(
                                fun(U,Acc) ->
                                    {ok, CurrAlbums} = maps:find(U,Acc),
                                    maps:update(U,sets:add_element(Name,CurrAlbums),Acc) 
                                end,
                            AccessesByPerson,UsersAdded),
                            __AccessesByPerson = sets:fold(
                                fun(U,Acc) ->
                                    {ok, CurrAlbums} = maps:find(U,Acc),
                                    maps:update(U,sets:del_element(Name,CurrAlbums),Acc) 
                                end,
                            _AccessesByPerson,UsersRemoved),
                            _Albums = maps:update(Name,{PID,InUse,_Users},Albums),
                            From ! {?MODULE, {ok}},
                            loop(_Albums,__AccessesByPerson);
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end;

        {{leave,Username,Name,Clock,Position}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,InUse,Users}} ->
                    case InUse of
                        false -> 
                            From ! {?MODULE, {error, "Can't leave without first asking"}},
                            loop(Albums,AccessesByPerson);
                        _Username ->
                            if
                                _Username == Username ->
                                    _Albums = maps:update(Name,{PID,false,Users},Albums),
                                    album_manager:addClock(PID,Clock,Position),
                                    From ! {?MODULE, {ok}},
                                    loop(_Albums,AccessesByPerson);
                                true ->
                                    From ! {?MODULE, {error, "Can't leave another user is leaving"}},
                                    loop(Albums,AccessesByPerson)
                            end
                    end
            end;
        {{edit,Username,Name,Adress,Port}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {ok,{PID,InUse,Users}} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            if 
                                InUse == true -> 
                                    From ! {?MODULE, {error, "Can't connect at the moment"}},
                                    loop(Albums,AccessesByPerson);
                                true -> %else
                                    _Users = maps:update(Username,{Adress,Port},Users),
                                    _Albums = maps:update(Name,{PID,InUse,_Users},Albums),
                                    _Online = getOnline(Users),
                                    if
                                        _Online == [] ->
                                            {ok,Files} = album_manager:getAlbum(PID),
                                            From ! {?MODULE, {files, Files,maps:keys(Users)}},
                                            loop(_Albums,AccessesByPerson);
                                        true ->
                                            {Clock,Position} = album_manager:getClock(PID),
                                            From ! {?MODULE, {online, _Online,Clock,Position}},
                                            loop(_Albums,AccessesByPerson)
                                    end
                            end;
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end
    end.