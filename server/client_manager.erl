-module(client_manager).

-export([loop/2]).

% AUTHENTICATED REQUESTS

% processo em loop à espera de pedidos do user
loop(Socket,Username) ->
    receive
        {tcp, Socket, Bin} ->
            inet:setopts(Socket, [{active, once}]),
            Msg = messages:decode_msg(Bin,'Message'),
            case maps:get(type,Msg) of
                'ALBUMSLIST' -> albumsListHandler(Socket, Username);
                'LOGOUT' -> logoutHandler(Socket, Username);
                'ALBUMCREATE' -> albumCreateHandler(Socket,Username,maps:get(name,Msg))
            end;
        % if socket closes logout Client
        _ -> logoutHandler(Socket,Username)
    end.

% logout Client
logoutHandler(Socket, Username) ->
    case account_manager:logout(Username) of
        ok ->
            io:fwrite("Logged out user: ~p.\n", [Username]),
            answer_manager:respond(Socket,true,"Logged out"),
            authenticator:authentication(Socket); % unauthenticatedArea
        {error, ErrorMsg} ->
            io:fwrite("~p ~p\n", [ErrorMsg, Username]),
            answer_manager:respond(Socket,false,ErrorMsg),
            loop(Socket,Username)
    end. 


albumsListHandler(Socket, Username) ->
    io:fwrite("Getting list of albums user: ~p.\n", [Username]),
    case album_manager:listAlbums(Username) of
        {ok, Albums} ->
            io:fwrite("found albums for user: ~p.\n", [Username]),
            answer_manager:respond(Socket,true,Albums),
            loop(Socket,Username);
        {error, ErrorMsg} ->
            io:fwrite("~p ~p\n", [ErrorMsg, Username]),
            answer_manager:respond(Socket,false,ErrorMsg),
            loop(Socket,Username)
    end. 

albumCreateHandler(Socket,Username,Name) ->
    io:fwrite("Creating new album: ~p.\n", [Name]),
    case album_manager:createAlbum(Username,Name) of
        ok ->
            io:fwrite("New album created: ~p.\n", [Name]),
            answer_manager:respond(Socket,true,Name),
            loop(Socket,Username);
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:respond(Socket,false,ErrorMsg),
            loop(Socket,Username)
    end.