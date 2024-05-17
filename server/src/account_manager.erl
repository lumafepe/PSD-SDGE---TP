-module(account_manager).
-export([start/0, signup/2, login/4, logout/1,getAddress/1,addAlbum/2,removeAlbum/2,getAlbums/1]).


% inicia o gestor de contas e regista o processo como account_manager
start() ->
    Users = #{}, % Username => {Password,{Ip,Port},set(albuns)}
    register(?MODULE, spawn(fun() -> loop(Users) end)). 


rpc(Request) -> 
    ?MODULE ! {Request,self()},
    receive
        {?MODULE, Result} -> Result
    end.

signup(Username,Password) ->
    rpc({signup,Username,Password}).

login(Username,Password,Ip,Port) ->
    rpc({login,Username,Password,Ip,Port}).

getAddress(Username) ->
    rpc({get,Username}).

getAlbums(Username) ->
    rpc({getAlbums,Username}).

logout(Username) ->
    rpc({logout,Username}).

addAlbum(Username,Album) ->
    rpc({addAlbum,Username,Album}).

removeAlbum(Username,Album) ->
    rpc({removeAlbum,Username,Album}).


% Accounts = Map{Username => {Password, loggedIn_flag, albums_set}}
loop(Accounts) -> 
    receive
        {{signup,Username,Password},From} ->
            case maps:find(Username,Accounts) of 
                error -> 
                    From ! {?MODULE, ok},
                    loop(maps:put(Username,{Password,false,sets:new()}, Accounts));
                _ -> 
                    From ! {?MODULE, {error, username_taken}},
                    loop(Accounts)
            end;

        {{login,Username,Password,Ip,Port},From} -> 
            case maps:find(Username,Accounts) of 
                {ok, {Password,false,Albums}} -> 
                    From ! {?MODULE, {ok}},
                    loop(maps:update(Username,{Password,{Ip,Port},Albums},Accounts));
                {ok, {Password,_,_}} -> 
                    From ! {?MODULE, {error, login_status, "User already logged in"}},
                    loop(Accounts);
                _ -> 
                    From ! {?MODULE, {error, invalid_credentials, "Invalid credentials"}},
                    loop(Accounts)
            end;

        {{logout,Username},From} ->
            case maps:find(Username,Accounts) of 
                {ok, {Password,{_,_},Albums}} -> 
                    From ! {?MODULE, ok},
                    loop(maps:update(Username,{Password,false,Albums},Accounts));
                _ -> 
                    From ! {?MODULE, {error, "User not logged in"}},
                    loop(Accounts)
            end;

        {{get,Username},From} ->
            case maps:find(Username,Accounts) of
            {ok, {_,Data,_}} ->
                From ! {?MODULE, {ok,Data}},
                loop(Accounts);
            _ ->
                From ! {?MODULE, {error}},
                loop(Accounts)
            end;
        {{addAlbum,Username,Album},From} ->
            case maps:find(Username,Accounts) of
            {ok, {Password,Data,Albums}} ->
                From ! {?MODULE, ok},
                loop(maps:update(Username,{Password,Data,sets:add_element(Album,Albums)},Accounts));
            _ ->
                From ! {?MODULE, {error,"user not found"}},
                loop(Accounts)
            end;
        {{removeAlbum,Username,Album},From} ->
            case maps:find(Username,Accounts) of
            {ok, {Password,Data,Albums}} ->
                From ! {?MODULE, ok},
                loop(maps:update(Username,{Password,Data,sets:del_element(Album,Albums)},Accounts));
            _ ->
                From ! {?MODULE, {error,"user not found"}},
                loop(Accounts)
            end;
        {{getAlbums,Username},From} ->
            case maps:find(Username,Accounts) of
            {ok, {_,_,Albums}} ->
                From ! {?MODULE, {ok,Albums}},
                loop(Accounts);
            _ ->
                From ! {?MODULE, {error,"user not found"}},
                loop(Accounts)
            end
    end.