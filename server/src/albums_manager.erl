-module(album_manager).

-export([start/0, listAlbums/1, addUser/1, createAlbum/2,editAlbum/4,isLast/2,leave/3]).

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

leave(Username,Name,Clock) ->
    rpc({leave,Username,Name,Clock}).

getOnline(Users) ->
    Status = maps:values(Users),
    lists:filter(
        fun(State)-> 
            case State of
                false ->
                    false;
                {_,_} ->
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
                    {ok, _CurrSet} = maps:get(Username,AccessesByPerson),
                    _AccessesByPerson = maps:update(Username,sets:put(Name,_CurrSet),AccessesByPerson),
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
                {PID,InUse,Users} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            case InUse of
                                false ->
                                    _Online = getOnline(Users),
                                    _Albums = maps:update(Name,{PID,Username,lists:delete(Username,Users)},Albums),
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
        {{leave,Username,Name,Clock}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {PID,InUse,Users} ->
                    {ok,PermissionSet} = maps:find(Username,AccessesByPerson),
                    HasPerms = sets:is_element(Name,PermissionSet),
                    if
                        HasPerms ->
                            case InUse of
                                false -> 
                                    From ! {?MODULE, {error, "Can't leave without first asking"}},
                                    loop(Albums,AccessesByPerson);
                                _Username -> %else
                                    if
                                        _Username == InUse ->
                                            _Users = maps:update(Username,false,Users),
                                            _Albums = maps:update(Name,{PID,false,_Users}),
                                            Clock = album_manager:addClock(PID,Clock),
                                            From ! {?MODULE, {ok}},
                                            loop(_Albums,AccessesByPerson);
                                        true ->
                                            From ! {?MODULE, {error, "Can't leave another user is leaving"}},
                                            loop(Albums,AccessesByPerson)
                                    end
                            end;
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end;
        {{edit,Username,Name,Adress,Port}, From} ->
            case maps:find(Name,Albums) of 
                error ->
                    From ! {?MODULE, {error, "Album doesn't exists"}},
                    loop(Albums,AccessesByPerson);
                {PID,InUse,Users} ->
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
                                    _Albums = maps:update(Name,{PID,InUse,_Users}),
                                    _Online = getOnline(Users),
                                    if
                                        _Online == [] ->
                                            Files = album_manager:getAlbum(PID),
                                            From ! {?MODULE, {files, Files,maps:keys(Users)}},
                                            loop(_Albums,AccessesByPerson);
                                        true ->
                                            Clock = album_manager:getClock(PID),
                                            From ! {?MODULE, {online, _Online,Clock}},
                                            loop(_Albums,AccessesByPerson)
                                    end
                            end;
                        true ->
                            From ! {?MODULE, {error, "User doesn't have permission"}},
                            loop(Albums,AccessesByPerson)
                    end
            end
    end.