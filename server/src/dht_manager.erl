-module(dht_manager).
-export([start/0,startEntrance/3, endEntrance/3, read/1, write/1]).


sha1_to_int(SHA1Str) ->
    SHA1Bin = hexstr_to_bin(SHA1Str),
    <<Integer:160>> = SHA1Bin,
    Integer.

hexstr_to_bin(HexStr) ->
    BinStr = hex:decode(HexStr),
    <<BinStr/binary>>.

binary_search(List, _, Low, High) when Low > High -> lists:nth(1, List) ;  % If low > high, return first.
binary_search(List, Value, Low, High) ->
    Mid = (Low + High) div 2,
    Val = sha1_to_int(lists:nth(Mid, List)),
    if 
        Val >= Value ->
            Prev = sha1_to_int(lists:nth(Mid-1, List)),
            if 
                ( Val==Value or (Prev < Value)) -> 
                    lists:nth(Mid, List);
                true ->
                    binary_search(List, Value, Low, Mid - 1)
            end;
        true ->
            binary_search(List, Value, Mid + 1, High)
    end.
    

findNearest(Hashdict, Hash, Write) ->
    _Hashdict = orddict:filter(
        fun (_, Vals) -> 
            Selected = sets:filter(fun({_,_,V}) -> V or Write end,Vals),
            not sets:is_empty(Selected)
        end
    ,Hashdict),
    binary_search(orddict:fetch_keys(_Hashdict), sha1_to_int(Hash), 1, orddict:size(_Hashdict)).



start() ->
    Nodes = #{},
    Sections = orddict:from_list([]),
    Joining = false,
    register(?MODULE, spawn(fun() -> loop(Nodes,Sections,Joining) end)).

startEntrance(Ip,Port,Tokens) ->
    rpc({startEntrance,Ip,Port,Tokens}).

endEntrance(Ip,Port,Tokens) ->
    rpc({endEntrance,Ip,Port,Tokens}).

read(Token) ->
    rpc({read,Token}).
write(Token) ->
    rpc({write,Token}).

rpc(Request) -> 
    ?MODULE ! {Request,self()},
    receive
        {?MODULE, Result} -> Result
    end.



%Nodes = Map(hash => {ip,port})
loop(Nodes,Sections,Joining) -> 
    receive
        {{startEntrance,Ip,Port,Tokens},From} ->
            case Joining of
                false ->
                    _Nodes = maps:put({Ip,Port},sets:from_list(Tokens),Nodes),
                    _Sections = lists:foldl(
                        fun(Token,Secs) ->
                            case orddict:find(Token,Secs) of
                                {ok, Set} ->
                                    orddict:store(Token,sets:add_element({Ip,Port,false},Set),Secs);
                                _ ->
                                    orddict:store(Token,sets:from_list([{Ip,Port,false}]),Secs)
                            end
                        end,
                    Sections,Tokens),
                    From ! {?MODULE, {ok, _Nodes} },
                    loop(_Nodes,_Sections,true);
                true ->
                    From ! {?MODULE, {error, "Can't enter new node while another is joining"}},
                    loop(Nodes,Sections,true)
            end;
        {{endEntrance,Ip,Port,Tokens},From} ->
            TokensSet = sets:from_list(Tokens),
            case Joining of
                false ->
                    From ! {?MODULE, {error, "Can't finish joining a new node, no node joined"}},
                    loop(Nodes,Sections,false);
                true ->
                    ServersIPs = lists:flatmap(
                        fun(Token) ->
                            {ok,S} = orddict:find(Token,Sections),
                            lists:map(
                                fun({_Ip,_Port,_}) -> 
                                    {_Ip,_Port} 
                                end,
                            sets:to_list(S))
                        end,
                    Tokens),
                    _Nodes = lists:foldl(
                        fun(ServerIP,NodesAcc) ->
                            {ok, ServerTokens} = maps:find(ServerIP,NodesAcc),
                            maps:put(ServerIP,sets:subtract(ServerTokens,TokensSet),NodesAcc)
                        end,
                        Nodes,ServersIPs
                    ),
                    _Sections = lists:foldl(
                        fun(Token,Acc) ->
                            orddict:store(Token,sets:from_list([{Ip,Port,true}]),Acc)
                        end,
                    Sections,Tokens),
                    __Nodes = maps:update({Ip,Port},TokensSet,_Nodes),
                    From ! {?MODULE, ok },
                    loop(__Nodes,_Sections,false)
            end;
        {{read,Token},From} ->
            case orddict:size(orddict:filter( fun(_,S) -> sets:fold(fun({_,_,V},Acc)-> Acc or V end,false,S) end,Sections)) of
                0 -> 
                    From ! {?MODULE, {error, "No servers connected"} },
                    loop(Nodes,Sections,Joining);
                _ ->
                    _Hash = findNearest(Sections,Token,false),
                    From ! {?MODULE, {ok, orddict:fetch(_Hash,Sections)}},
                    loop(Nodes,Sections,Joining)
            end;
        {{write,Token},From} ->
            case orddict:size(Sections) of
                0 -> 
                    From ! {?MODULE, {error, "No servers connected"} },
                    loop(Nodes,Sections,Joining);
                _ ->
                    _Hash = findNearest(Sections,Token,true),
                    From ! {?MODULE, {ok, orddict:fetch(_Hash,Sections)}},
                    loop(Nodes,Sections,Joining)
            end
    end.
