-module(client_manager).

-export([loop/3]).

% AUTHENTICATED REQUESTS


getAlbumName(Msg) ->
    maps:get(album_name,Msg).
getAlbumUsernames(Msg) ->
    maps:get(users,maps:get(album,Msg)).

getAlbumFiles(Msg) ->
    FilesList = maps:get(files,maps:get(album,Msg)), % List of maps
    maps:from_list(lists:map(
        fun(File) -> 
            ClassificationsList = maps:get(classifications,File),
            Classifications = maps:from_list(lists:map(fun(Class)-> {maps:get(username,Class),maps:get(value,Class)} end,ClassificationsList)),
            {maps:get(name,File),{maps:get(hash,File),Classifications}}
        end,
    FilesList)).

getDHTToken(Msg) ->
    maps:get(token,Msg).

getLeaveClock(Msg) ->
    maps:get(clock,maps:get(leave_data,Msg)).
    
getLeavePosition(Msg) ->
    maps:get(position,maps:get(leave_data,Msg)).


% processo em loop à espera de pedidos do user
loop(Socket, CurrAlbumPid, Username) ->
    receive
        {tcp, Socket, Bin} ->
            inet:setopts(Socket, [{active, once}]),
            Msg = messages:decode_msg(Bin,'Message'),
            case maps:get(type,Msg) of
                'ALBUMSLIST' -> albumsListHandler(Socket, CurrAlbumPid, Username);
                'LOGOUT' -> logoutHandler(Socket, CurrAlbumPid, Username);
                'ALBUMCREATE' -> albumCreateHandler(Socket, CurrAlbumPid, Username,getAlbumName(Msg));
                'ALBUMEDIT' -> albumEditHandler(Socket, CurrAlbumPid, Username,getAlbumName(Msg));     
                'LEAVE' -> albumLeaveHandler(Socket, CurrAlbumPid, Username,getAlbumName(Msg),getLeaveClock(Msg),getLeavePosition(Msg),getAlbumUsernames(Msg),getAlbumFiles(Msg));
                'READ' -> readHandler(Socket, CurrAlbumPid, Username,getDHTToken(Msg));
                'WRITE' -> writeHandler(Socket, CurrAlbumPid, Username,getDHTToken(Msg))
            end;
        % if socket closes logout Client
        _ -> logoutHandler(Socket, CurrAlbumPid, Username)
    end.

% logout Client
logoutHandler(Socket, CurrAlbumPid, Username) ->
    case account_manager:logout(Username) of
        ok ->
            io:fwrite("Logged out user: ~p.\n", [Username]),
            answer_manager:success(Socket),
            ok;
        {error, ErrorMsg} ->
            io:fwrite("~p ~p\n", [ErrorMsg, Username]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket, CurrAlbumPid, Username)
    end. 


albumsListHandler(Socket, CurrAlbumPid, Username) ->
    io:fwrite("Getting list of albums user: ~p.\n", [Username]),
    {ok, Albums} = account_manager:getAlbums(Username),
    io:fwrite("found albums for user: ~p.\n", [Username]),
    answer_manager:listAlbums(Socket,Albums),
    loop(Socket, CurrAlbumPid, Username).

albumCreateHandler(Socket, CurrAlbumPid, Username,Name) ->
    io:fwrite("Creating new album: ~p.\n", [Name]),
    case albums_manager:createAlbum(Name,Username) of
        ok ->
            io:fwrite("New album created: ~p.\n", [Name]),
            account_manager:addAlbum(Username,Name),
            answer_manager:success(Socket),
            loop(Socket, CurrAlbumPid, Username);
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket, CurrAlbumPid, Username)
    end.


albumEditHandler(Socket, CurrAlbumPid, Username,Name) ->
    io:fwrite("Starting to edit album: ~p.\n", [Name]),
    {ok, {Adress,Port}} = account_manager:getAddress(Username),
    case albums_manager:getAlbum(Name) of
        {error, ErrorMsg} ->
            io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket, CurrAlbumPid, Username);
        {ok,PID} ->
            case album_manager:editAlbum(PID,Username,Adress,Port) of
                {files, Files,Users} ->
                    io:fwrite("album fetched: ~p and was first.\n", [Name]),
                    answer_manager:album(Socket,Files,Users),
                    loop(Socket, PID, Username);
                {online, _Online,Clock,Position} ->
                    io:fwrite("album fetched: ~p and had clients\n", [Name]),
                    answer_manager:new_client(Socket,_Online,Clock,Position),
                    loop(Socket, PID, Username);
                {error, ErrorMsg} ->
                    io:fwrite("~p ~p.\n", [ErrorMsg, Name]),
                    answer_manager:errorReply(Socket,ErrorMsg),
                    loop(Socket, CurrAlbumPid, Username)
            end
    end.

albumLeaveHandler(Socket, CurrAlbumPid, Username,Name,Clock,Position,Users,Files) ->
    io:fwrite("Finishing leaving process: album:~p user:~p.\n", [Name,Username]),
    album_manager:leaveAlbum(CurrAlbumPid,Username,Clock,Position,Users,Files),
    io:fwrite("Left Album: ~p.\n", [Name]),
    answer_manager:success(Socket),
    loop(Socket, none, Username).




readHandler(Socket, CurrAlbumPid, Username, Token) ->
    case dht_manager:read(Token) of
        {error, ErrorMsg} ->
            io:fwrite("Failed get read Node: ~p, ~p.\n", [Token,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket, CurrAlbumPid, Username);
        {ok, Data} ->
            io:fwrite("Found read Node for hash: ~p.\n", [Token]),
            answer_manager:server(Socket,Data),
            loop(Socket, CurrAlbumPid, Username)
    end.

writeHandler(Socket, CurrAlbumPid, Username, Token) ->
    case dht_manager:write(Token) of
        {error, ErrorMsg} ->
            io:fwrite("Failed get write Node: ~p, ~p.\n", [Token,ErrorMsg]),
            answer_manager:errorReply(Socket,ErrorMsg),
            loop(Socket, CurrAlbumPid, Username);
        {ok, Data} ->
            io:fwrite("Found write Node for hash: ~p.\n", [Token]),
            answer_manager:server(Socket,Data),
            loop(Socket, CurrAlbumPid, Username)
end.




