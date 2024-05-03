-module(album_manager).

-export([start/0, getAlbum/1, writeAlbum/2, addClock/2, getClock/1]).

start() ->
    Files = #{}, %{Nome:{HASH,{user:valor }}}
    Vectors = [],
    loop(Files,Vectors).

rpc(PID,Request) ->
    PID ! {Request,self()},
    receive
        {PID, Result} -> Result
    end.

getAlbum(PID) ->
    rpc(PID,{get}).
writeAlbum(PID,Files) ->
    rpc(PID,{write,Files}).
addClock(PID,Clock) ->
    rpc(PID,{addClock,Clock}).
getClock(PID) ->
    rpc(PID,{getClock}).
    

loop(Files,Vectors) ->
    receive
        {{get}, From} ->
            From ! {?MODULE, {ok, Files}},
            loop(Files,Vectors);
        {{write,_Files}, From} ->
            From ! {?MODULE, {ok}},
            loop(_Files,Vectors);
        {{addClock,Clock}, From} ->
            _Vectors = lists:append([Vectors,[Clock]]),
            From ! {?MODULE, {ok}},
            loop(Files,_Vectors);
        {{getClock}, From} ->
            if 
                Vectors==[] ->
                    From ! {?MODULE, {error,"Vector clock is empty"}},
                    loop(Files,Vectors);
                true ->
                    Clock = lists:last(Vectors),
                    _Vectors = lists:droplast(Vectors),
                    From ! {?MODULE, {ok,Clock}},
                    loop(Files,_Vectors)
            end
    end.

    
