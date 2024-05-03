-module(authentication).

-export([authenticate/1]).

% NON AUTHENTICATED AREA

% loop awaiting authentication requests
authenticate(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            inet:setopts(Socket, [{active, once}]),
            Msg = messages:decode_msg(Bin,'Message'),
            case maps:get(type,Msg) of
                'REGISTER' -> registerHandler(Socket, maps:get(user_data,Msg));
                'LOGIN' -> loginHandler(Socket, maps:get(user_data,Msg));
                'STARTENTRANCE' -> startEntranceHandler(Socket, maps:get(node_info,Msg));
                'ENDENTRANCE' -> endEntranceHandler(Socket, maps:get(node_info,Msg));
                _ ->
                    answer_manager:errorReply(Socket,"Invalid request"),
                    authenticate(Socket)
            end;
        _ -> % client dies before authing
            bye
    end.



registerHandler(Socket, Data) ->
    Username = maps:get(username, Data),
    Password = maps:get(password, Data),
    case account_manager:signup(Username, Password) of % adiciona o novo user ao mapa do account_manager
        ok ->
            io:fwrite("Registered user: ~p ~p.\n", [Username, Password]),
            answer_manager:success(Socket),
            authenticate(Socket);
        _ ->
            io:fwrite("Username already taken: ~p.\n", [Username]),
            answer_manager:errorReply(Socket,"UsernameTaken"),
            authenticate(Socket)
    end.


loginHandler(Socket, Data) ->
    Username = maps:get(username, Data),
    Password = maps:get(password, Data),
    case account_manager:login(Username, Password) of
        {ok} ->
            io:fwrite("Logged in user: ~p.\n", [Username]),
            answer_manager:success(Socket),
            client_manager:loop(Socket,Username); % authenticated area
        {error, TypeError, ErrorMsg} ->
            case TypeError of % tipos de erro possíveis de login
                login_status -> io:fwrite("User already logged in: ~p.\n", [Username]);
                invalid_credentials -> io:fwrite("The provided credentials where incorrect: ~p ~p.\n", [Username, Password])
            end,
            answer_manager:errorReply(Socket,ErrorMsg),
            authenticate(Socket)
    end.

startEntranceHandler(Socket, Data)->
    Ip = maps:get(ip,Data),
    Port = maps:get(port,Data),
    Tokens = maps:get(tokens,Data),
    case dht_manager:startEntrance(Ip, Port, Tokens) of 
        {ok, Nodes} ->
            io:fwrite("Node started connecting: ~p:~p.\n", [Ip,Port]),
            answer_manager:nodes(Socket,Nodes),
            authenticate(Socket);
        {error, ErrorMsg} -> 
            io:fwrite("Failed Node start connecting: ~p:~p, ~p.\n", [Ip,Port,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            authenticate(Socket)
    end.

endEntranceHandler(Socket, Data)->
    Ip = maps:get(ip,Data),
    Port = maps:get(port,Data),
    Tokens = maps:get(tokens,Data),
    case dht_manager:endEntrance(Ip, Port, Tokens) of 
        ok ->
            io:fwrite("Node connected: ~p:~p.\n", [Ip,Port]),
            answer_manager:success(Socket),
            authenticate(Socket);
        {error, ErrorMsg} -> 
            io:fwrite("Failed Node connect: ~p:~p, ~p.\n", [Ip,Port,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            authenticate(Socket)
end.