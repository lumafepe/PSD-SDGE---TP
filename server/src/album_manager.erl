-module(album_manager).

-export([start/2, editAlbum/4,leaveAlbum/6]).

start(Name,Username) ->
    Files = #{}, %{Nome:{HASH,{user:valor }}}
    Vectors = #{0=>0}, %{Position : Value}
    AvaiableVectors = sets:new(), % positions
    UsersOnline = #{}, % {Username => {Ip,Porta}}
    UsersPermission = sets:from_list([Username]),
    loop(Name,Files,Vectors,AvaiableVectors,UsersOnline,UsersPermission).


rpc(PID,Request) ->
    PID ! {Request,self()},
    receive
        {_, Result} -> Result
    end.


leaveAlbum(PID,Username,Clock,Position,NewUsers,_Files) ->
    rpc(PID,{leave,Username,Clock,Position,NewUsers,_Files}).

editAlbum(PID,Username,Adress,Port) ->
    rpc(PID,{edit,Username,Adress,Port}).

popSet(Set) ->
    {{X},Y} = sets:fold(
        fun(X,Acc) ->
            case Acc of
                {false,S} -> {{X},S};
                {A,S} -> {A,sets:add_element(X,S)}
            end
        end,
    {false,sets:new()},Set),
    {X,Y}.


getClock(AvaiableVectors,Vectors) ->
    NoVectorsFree = sets:is_empty(AvaiableVectors),
    if 
        NoVectorsFree ->
            NewPosition = length(maps:to_list(Vectors)),
            _Vectors = maps:put(NewPosition,0,Vectors),
            {_Vectors,AvaiableVectors,0,NewPosition};
        true ->
            {Position,_AvaiableVectors} = popSet(AvaiableVectors),
            {ok,Clock} = maps:find(Position,Vectors),
            {Vectors,_AvaiableVectors,Clock,Position}
    end.


getUsersRemoved(Old,New) ->
    sets:subtract(Old,New).

getUsersAdded(Old,New) ->
    sets:subtract(New,Old).

loop(Name,Files,Vectors,AvaiableVectors,UsersOnline,UsersPermission) ->
    receive
        {{leave,Username,Clock,Position,NewUsers,_Files}, From} ->
            _AvaiableVectors = sets:add_element(Position,AvaiableVectors),
            _Vectors = maps:update(Position,Clock,Vectors),
        
            UsersAdded = getUsersAdded(UsersPermission,sets:from_list(NewUsers)),
            UsersRemoved = getUsersRemoved(UsersPermission,sets:from_list(NewUsers)),
            
            lists:foreach(fun(User) ->account_manager:addAlbum(User,Name) end,sets:to_list(UsersAdded)), 
            lists:foreach(fun(User) ->account_manager:removeAlbum(User,Name) end,sets:to_list(UsersRemoved)),

            _UsersPermission = sets:subtract(UsersPermission,UsersRemoved),
            __UsersPermission = sets:union(_UsersPermission,UsersAdded),
        
            _UsersOnline = maps:remove(Username,UsersOnline),
            From ! {?MODULE, {ok}},
            loop(Name,_Files,_Vectors,_AvaiableVectors,_UsersOnline,__UsersPermission);
        
        {{edit,Username,Adress,Port}, From} ->
            HasPerm = sets:is_element(Username,UsersPermission),
            if 
                HasPerm ->
                    _UsersOnline = maps:put(Username,{Adress,Port},UsersOnline),
                    _Online = maps:values(UsersOnline),
                    if
                        _Online == [] ->
                            From ! {?MODULE, {files, Files,sets:to_list(UsersPermission)}},
                            loop(Name,Files,#{0=>0},sets:new(),_UsersOnline,UsersPermission);
                        true ->
                            {_Vectors,_AvaiableVectors,Clock,Position} = getClock(AvaiableVectors,Vectors),
                            From ! {?MODULE, {online, UsersOnline,Clock,Position}},
                            loop(Name,Files,_Vectors,_AvaiableVectors,_UsersOnline,UsersPermission)
                    end;
                true ->
                    From ! {?MODULE, {error, "User doesn't have permission"}},
                    loop(Name,Files,Vectors,AvaiableVectors,UsersOnline,UsersPermission)
            end
    end.

    
