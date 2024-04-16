-module(dht_manager).
-export([start/0,startEntrance/3, endEntrance/3, read/1, write/1]).

str2Hash(Str) ->
    list_to_integer(Str, 16).




binary_search(List, _, Low, High) when Low > High -> lists:nth(1, List) ;  % If low > high, return first.
binary_search(List, Value, Low, High) ->
    Mid = (Low + High) div 2,
    Val = lists:nth(Mid, List),
    Prev = lists:nth(Mid-1, List),
    if 
        Val >= Value ->
            if 
                ( Val==Value or (Prev < Value)) -> 
                    Val;
                true ->
                    binary_search(List, Value, Low, Mid - 1)
            end;
        true ->
            binary_search(List, Value, Mid + 1, High)
    end.
    

findNearest(Hashdict, Hash, Write) ->
    _Hashdict = orddict:filter(fun (_, {_,_,V}) -> V or Write end,Hashdict),
    binary_search(orddict:fetch_keys(_Hashdict), Hash, 1, orddict:size(_Hashdict)).

start() ->
    Nodes = #{ 
        {"localhost","1234"} => sets:from_list([str2Hash("28b92b56ee64b92ebb72d865f172ef00c708df83"),str2Hash("097d77c23f92f9125db3aab09f23fc6b3401e2b0"),str2Hash("74ad1db8d2b1da079f3b8f3f93807e907c9f0f3f"),str2Hash("72019bbac0b3dac88beac9ddfef0ca808919104f"),str2Hash("0213e69386181c759ffa6dfb5d4911b5088163f8")])
    },
    Sections = orddict:from_list([
        {str2Hash("0213e69386181c759ffa6dfb5d4911b5088163f8"),{"localhost","1234",true}},
        {str2Hash("097d77c23f92f9125db3aab09f23fc6b3401e2b0"),{"localhost","1234",true}},
        {str2Hash("28b92b56ee64b92ebb72d865f172ef00c708df83"),{"localhost","1234",true}},
        {str2Hash("72019bbac0b3dac88beac9ddfef0ca808919104f"),{"localhost","1234",true}},
        {str2Hash("74ad1db8d2b1da079f3b8f3f93807e907c9f0f3f"),{"localhost","1234",true}}
    ]),
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
                    _Tokens = lists:map(fun(X) -> str2Hash(X) end,Tokens),
                    _Nodes = maps:put({Ip,Port},sets:from_list(_Tokens),Nodes),
                    _Sections = lists:foldr(fun(V,Acc) ->orddict:store(V,{Ip,Port,false},Acc) end,Sections,_Tokens),
                    From ! {?MODULE, {ok, _Nodes} },
                    loop(_Nodes,_Sections,true);
                true ->
                    From ! {?MODULE, {error, "Can't enter new node while another is joining"}},
                    loop(Nodes,Sections,true)
            end;
        {{endEntrance,Ip,Port,Tokens},From} ->
            case Joining of
                false ->
                    From ! {?MODULE, {error, "Can't finish joining a new node, no node joined"}},
                    loop(Nodes,Sections,false);
                true ->
                    _Tokens = lists:map(fun(X) -> str2Hash(X) end,Tokens),
                    _Sections = lists:foldr(fun(V,Acc) ->orddict:store(V,{Ip,Port,true},Acc) end,Sections,_Tokens),
                    From ! {?MODULE, ok },
                    loop(Nodes,_Sections,false)
            end;
        {{read,Token},From} ->
            case orddict:size(orddict:filter(fun(_,{_, _, V}) -> V end,Sections)) of
                0 -> 
                    From ! {?MODULE, {error, "No servers connected"} },
                    loop(Nodes,Sections,Joining);
                _ ->
                    _Hash = findNearest(Sections,str2Hash(Token),false),
                    From ! {?MODULE, {ok, orddict:fetch(_Hash,Sections)}},
                    loop(Nodes,Sections,Joining)
            end;
        {{write,Token},From} ->
            case orddict:size(Sections) of
                0 -> 
                    From ! {?MODULE, {error, "No servers connected"} },
                    loop(Nodes,Sections,Joining);
                _ ->
                    _Hash = findNearest(Sections,str2Hash(Token),true),
                    From ! {?MODULE, {ok, orddict:fetch(_Hash,Sections)}},
                    loop(Nodes,Sections,Joining)
            end
    end.