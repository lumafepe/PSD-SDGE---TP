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
                'ALBUMCREATE' -> albumCreateHandler(Socket,Username,maps:get(name,maps:get(album_name,Msg)));
                'ALBUMEDIT' -> albumEditHandler(Socket,Username,maps:get(name,maps:get(album_name,Msg)));
                'ISLAST' -> albumIsLastHandler(Socket,Username,maps:get(name,maps:get(album_name,Msg)));
                'LEAVE' -> albumLeaveHandler(Socket,Username,maps:get(name,maps:get(leave_data,Msg)),maps:get(clock,maps:get(leave_data,Msg)));
                'READ' -> readHandler(Socket,Username,maps:get(token,Msg));
                'WRITE' -> writeHandler(Socket,Username,maps:get(token,Msg))
            end;
        % if socket closes logout Client
        _ -> logoutHandler(Socket,Username)
    end.

% logout Client
logoutHandler(Socket, Username) ->
    case account_manager:logout(Username) of
        ok ->
            io:fwrite("Logged out user: ~p.\n", [Username]),
            answer_manager:success(Socket),
            authenticator:authentication(Socket); % unauthenticatedArea
        {error, ErrorMsg} ->
            io:fwrite("~p ~p\n", [ErrorMsg, Username]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username)
    end. 


albumsListHandler(Socket, Username) ->
    io:fwrite("Getting list of albums user: ~p.\n", [Username]),
    {ok, Albums} = album_manager:listAlbums(Username), %set(album_Name)
    io:fwrite("found albums for user: ~p.\n", [Username]),
    answer_manager:listAlbums(Socket,Albums),
    loop(Socket,Username).

albumCreateHandler(Socket,Username,Name) ->
    io:fwrite("Creating new album: ~p.\n", [Name]),
    case album_manager:createAlbum(Username,Name) of
        ok ->
            io:fwrite("New album created: ~p.\n", [Name]),
            answer_manager:success(Socket),
            loop(Socket,Username);
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username)
    end.

albumEditHandler(Socket,Username,Name) ->
    io:fwrite("Starting to edit album: ~p.\n", [Name]),
    {ok,{Adress,Port}} = inet:peername(Socket),
    case albums_manager:editAlbum(Username,Name,Adress,Port) of
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username);
        {files, Files,Users} ->
            io:fwrite("album fetched: ~p and was first.\n", [Name]),
            answer_manager:album(Socket,Files,Users),
            loop(Socket,Username);
        {online, _Online,Clock} ->
            io:fwrite("album fetched: ~p and had clients\n", [Name]),
            answer_manager:new_client(Socket,_Online,Clock),
            loop(Socket,Username)
    end.

albumIsLastHandler(Socket,Username,Name) ->
    io:fwrite("Starting leaving process: album:~p user:~p.\n", [Name,Username]),
    case albums_manager:isLast(Username,Name) of
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username);
        Value ->
            io:fwrite("client leaving album:~p is_last ~p.\n", [Name,Value]),
            answer_manager:is_last(Socket,Value),
            loop(Socket,Username)
    end.

albumLeaveHandler(Socket,Username,Name,Clock) ->
    io:fwrite("Finishing leaving process: album:~p user:~p.\n", [Name,Username]),
    case albums_manager:leave(Username,Name,Clock) of
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username);
        _ ->
            io:fwrite("Left Album: ~p.\n", [Name]),
            answer_manager:success(Socket),
            loop(Socket,Username)
    end.




readHandler(Socket,Username, Token) ->
    case dht_manager:read(Token) of
        {error, ErrorMsg} ->
            io:fwrite("Failed get read Node: ~p, ~p.\n", [Token,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username);
        {ok, Data} ->
            io:fwrite("Found read Node for hash: ~p.\n", [Token]),
            answer_manager:server(Socket,Data),
            loop(Socket,Username)
    end.

writeHandler(Socket,Username, Token) ->
    case dht_manager:write(Token) of
        {error, ErrorMsg} ->
            io:fwrite("Failed get write Node: ~p, ~p.\n", [Token,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket,Username);
        {ok, Data} ->
            io:fwrite("Found write Node for hash: ~p.\n", [Token]),
            answer_manager:server(Socket,Data),
            loop(Socket,Username)
end.
    