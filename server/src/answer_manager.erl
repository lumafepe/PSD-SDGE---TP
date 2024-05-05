-module(answer_manager).

-export([errorReply/2, success/1, listAlbums/2, album/3, nodes/2, server/2,new_client/4,is_last/2]).

% reply to request
errorReply(Socket,Msg) ->
    Reply = messages:encode_msg(#{type=>'ERRORREPLY', error_message => #{message=>Msg}}, 'Message'),
    gen_tcp:send(Socket,Reply).


success(Socket) ->
    Reply = messages:encode_msg(#{type=>'SUCESIUM'}, 'Message'),
    gen_tcp:send(Socket,Reply).



listAlbums(Socket,Albums_set) ->
    Reply = messages:encode_msg(#{type=>'ALBUMS',albums=>#{names => sets:to_list(Albums_set)}}, 'Message'),
    gen_tcp:send(Socket,Reply).



parseClassification(Username,Classification)->
    #{
        username=>Username,
        value=>Classification
    }.

parseFile(Name,Hash,Classifications) ->
    #{
        name=>Name,
        hash=>Hash,
        classifications=> lists:map(
            fun({Username,Classification}) ->
                parseClassification(Username,Classification)
            end,
        maps:to_list(Classifications))
    }.


album(Socket,Files,Users) ->
    % Files {Nome:{HASH,{user:valor }}}
    % Users [Username] 
    FilesParsed = lists:map(fun({Name,{Hash,Class}})->parseFile(Name,Hash,Class) end,maps:to_list(Files)),
    Reply = messages:encode_msg(#{type=>'ALBUM',album=>#{files=>FilesParsed,users=>Users}}, 'Message'),
    gen_tcp:send(Socket,Reply).


parseClient(Username,Ip,Port) ->
    #{
        username => Username,
        ip => Ip,
        port => Port
    }.

new_client(Socket,Clients,Clock,Position) ->
    % Clock int
    % Clients {Username:{IP,Port}}
    ParsedClients = lists:map(fun({Username,{Ip,Port}}) ->parseClient(Username,Ip,Port) end,maps:to_list(Clients)),
    Reply = messages:encode_msg(#{type=>'NEWCLIENT',new_client=>#{clients=>ParsedClients,clock=>Clock,position=>Position}}, 'Message'),
    gen_tcp:send(Socket,Reply).

is_last(Socket,Value) ->
    Reply = messages:encode_msg(#{type=>'ISLASTREPLY',is_last=>Value}, 'Message'),
    gen_tcp:send(Socket,Reply).



nodes(Socket,Nodes) ->
    _NodesAsListKV = maps:to_list(Nodes),
    _Nodes = lists:map(fun({{Ip,Porta},Tokens}) -> {Ip, Porta ,sets:to_list(Tokens)} end,_NodesAsListKV),
    Values = lists:map(fun({Ip,Porta,Tokens}) -> #{ip=>Ip,port=>Porta,tokens=>Tokens} end,_Nodes),
    Reply = messages:encode_msg(#{type=>'NODESINFO',nodes_infos=>Values}, 'Message'),
    gen_tcp:send(Socket,Reply).

server(Socket,Servers) ->
    Data=lists:map(fun({Ip, Port, _})->#{ip=>Ip,port=>Port} end,sets:to_list(Servers)),
    Reply = messages:encode_msg(#{type=>'NODEIP',nodes_ips=>Data}, 'Message'),
    gen_tcp:send(Socket,Reply).