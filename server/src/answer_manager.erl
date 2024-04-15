-module(answer_manager).

-export([errorReply/2, success/1, listAlbums/2, album/3, nodes/2]).

% reply to request
errorReply(Socket,Msg) ->
    Reply = messages:encode_msg(#{type=>'ERRORREPLY', reply => #{message=>Msg}}, 'Message'),
    gen_tcp:send(Socket,Reply).

success(Socket) ->
    Reply = messages:encode_msg(#{type=>'SUCESIUM'}, 'Message'),
    gen_tcp:send(Socket,Reply).

listAlbums(Socket,Albums_set) ->
    Reply = messages:encode_msg(#{type=>'ALBUMS',albums=>#{names => sets:to_list(Albums_set)}}, 'Message'),
    gen_tcp:send(Socket,Reply).

album(Socket,Name,Album) ->
    Reply = messages:encode_msg(#{type=>'ALBUM',album=>#{name=>Name,files=>sets:to_list(Album)}}, 'Message'),
    gen_tcp:send(Socket,Reply).


number_to_hex(Number) ->
    IntegerHex = integer_to_list(Number, 16),
    PadLength = 40 - length(IntegerHex),
    if
        PadLength > 0 ->
            ZeroPadding = string:copies("0", PadLength),
            ZeroPadding ++ IntegerHex;
        true ->
            IntegerHex
    end.


nodes(Socket,Nodes) ->
    _NodesAsListKV = maps:to_list(Nodes),
    _NodesAsList = lists:map(fun({{Ip,Porta},Tokens}) -> {Ip, Porta ,sets:to_list(Tokens)} end,_NodesAsListKV),
    _Nodes = lists:map(fun({Ip,Porta,Tokens}) -> {Ip, Porta,lists:map(fun(V) ->number_to_hex(V) end,Tokens)} end,_NodesAsList),
    Values = lists:map(fun({Ip,Porta,Tokens}) -> #{ip=>Ip,port=>Porta,tokens=>Tokens} end,_Nodes),
    Reply = messages:encode_msg(#{type=>'NODESINFO',nodesInfo=>Values}, 'Message'),
    gen_tcp:send(Socket,Reply).