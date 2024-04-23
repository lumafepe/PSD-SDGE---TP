-module(answer_manager).

-export([errorReply/2, success/1, listAlbums/2, album/3, nodes/2, server/2]).

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


nodes(Socket,Nodes) ->
    _NodesAsListKV = maps:to_list(Nodes),
    _Nodes = lists:map(fun({{Ip,Porta},Tokens}) -> {Ip, Porta ,sets:to_list(Tokens)} end,_NodesAsListKV),
    Values = lists:map(fun({Ip,Porta,Tokens}) -> #{ip=>Ip,port=>Porta,tokens=>Tokens} end,_Nodes),
    Reply = messages:encode_msg(#{type=>'NODESINFO',nodesInfo=>Values}, 'Message'),
    gen_tcp:send(Socket,Reply).

server(Socket,Servers) ->
    Data=lists:map(fun({Ip, Port, _})->#{ip=>Ip,port=>Port} end,sets:to_list(Servers)),
    Reply = messages:encode_msg(#{type=>'NODEIP',nodesIp=>Data}, 'Message'),
    gen_tcp:send(Socket,Reply).