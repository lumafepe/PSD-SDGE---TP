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
                'REGISTER' -> registerHandler(Socket, maps:get(register,Msg));
                'LOGIN' -> loginHandler(Socket, maps:get(login,Msg));
                _ ->
                    answer_manager:respond(Socket,false,"Invalid request"),
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
            answer_manager:respond(Socket,true,"UserRegistered"),
            authenticate(Socket);
        _ ->
            io:fwrite("Username already taken: ~p.\n", [Username]),
            answer_manager:respond(Socket,false,"UsernameTaken"),
            authenticate(Socket)
    end.


loginHandler(Socket, Data) ->
    Username = maps:get(username, Data),
    Password = maps:get(password, Data),
    case account_manager:login(Username, Password) of
        {ok} ->
            io:fwrite("Logged in user: ~p.\n", [Username]),
            answer_manager:respond(Socket,true,"Logged in"),
            client_manager:loop(Socket,Username); % authenticated area
        {error, TypeError, ErrorMsg} ->
            case TypeError of % tipos de erro possíveis de login
                login_status -> io:fwrite("User already logged in: ~p.\n", [Username]);
                invalid_credentials -> io:fwrite("The provided credentials where incorrect: ~p ~p.\n", [Username, Password])
            end,
            answer_manager:respond(Socket,false,ErrorMsg),
            authenticate(Socket)
    end.