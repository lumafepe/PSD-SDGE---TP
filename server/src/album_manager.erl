-module(album_manager).

-export([start/0, getAlbum/1, writeAlbum/2, addClock/3, getClock/1]).

start() ->
    Files = #{}, %{Nome:{HASH,{user:valor }}}
    Vectors = #{0=>0}, %{Position : Value}
    AvaiableVectors = sets:new(), % positions
    loop(Files,Vectors,AvaiableVectors).

rpc(PID,Request) ->
    PID ! {Request,self()},
    receive
        {_, Result} -> Result
    end.

getAlbum(PID) ->
    rpc(PID,{get}).
writeAlbum(PID,Files) ->
    rpc(PID,{write,Files}).
addClock(PID,Clock,Position) ->
    rpc(PID,{addClock,Clock,Position}).
getClock(PID) ->
    rpc(PID,{getClock}).
    


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

loop(Files,Vectors,AvaiableVectors) ->
    receive
        {{get}, From} ->
            From ! {?MODULE, {ok, Files}},
            loop(Files,#{0=>0},sets:new());
        {{write,_Files}, From} ->
            From ! {?MODULE, {ok}},
            loop(_Files,Vectors,AvaiableVectors);
        {{addClock,Clock,Position}, From} ->
            _AvaiableVectors = sets:add_element(Position,AvaiableVectors),
            _Vectors = maps:update(Position,Clock,Vectors),
            From ! {?MODULE, {ok}},
            loop(Files,_Vectors,_AvaiableVectors);
        {{getClock}, From} ->
            NoVectorsFree = sets:is_empty(AvaiableVectors),
            if 
                NoVectorsFree ->
                    NewPosition = length(maps:to_list(Vectors)),
                    _Vectors = maps:put(NewPosition,0,Vectors),
                    From ! {?MODULE, {0,NewPosition}},
                    loop(Files,_Vectors,AvaiableVectors);
                true ->
                    {Position,_AvaiableVectors} = popSet(AvaiableVectors),
                    {ok,Clock} = maps:find(Position,Vectors),
                    From ! {?MODULE, {Clock,Position}},
                    loop(Files,Vectors,_AvaiableVectors)
            end
    end.

    
