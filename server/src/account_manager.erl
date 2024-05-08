-module(account_manager).
-export([start/0, signup/2, login/4, logout/1,getAddress/1]).


% inicia o gestor de contas e regista o processo como account_manager
start() ->
    Users = #{
    },
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

logout(Username) ->
    rpc({logout,Username}).


% Accounts = Map{Username => {Password, loggedIn_flag, albums_set}}
loop(Accounts) -> 
    receive
        {{signup,Username,Password},From} ->
            case maps:find(Username,Accounts) of 
                error -> 
                    From ! {?MODULE, ok},
                    albums_manager:addUser(Username),
                    loop(maps:put(Username,{Password,false}, Accounts));
                _ -> 
                    From ! {?MODULE, {error, username_taken}},
                    loop(Accounts)
            end;
        {{login,Username,Password,Ip,Port},From} -> 
            case maps:find(Username,Accounts) of 
                {ok, {Password,false}} -> 
                    From ! {?MODULE, {ok}},
                    loop(maps:update(Username,{Password,{Ip,Port}},Accounts));
                {ok, {Password,_}} -> 
                    From ! {?MODULE, {error, login_status, "User already logged in"}},
                    loop(Accounts);
                _ -> 
                    From ! {?MODULE, {error, invalid_credentials, "Invalid credentials"}},
                    loop(Accounts)
            end;
        {{logout,Username},From} ->
            case maps:find(Username,Accounts) of 
                {ok, {Password,{_,_}}} -> 
                    From ! {?MODULE, ok},
                    loop(maps:update(Username,{Password,false},Accounts));
                _ -> 
                    From ! {?MODULE, {error, "User not logged in"}},
                    loop(Accounts)
            end;
        {{get,Username},From} ->
            case maps:find(Username,Accounts) of
            {ok, {Password,Data}} ->
                From ! {?MODULE, {ok,Data}},
                loop(Accounts);
            _ ->
                From ! {?MODULE, {error}},
                loop(Accounts)
            end
    end.