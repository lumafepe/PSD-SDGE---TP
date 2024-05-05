-module(account_manager).
-export([start/0, signup/2, login/2, logout/1]).


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

login(Username,Password) ->
    rpc({login,Username,Password}).

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
        {{login,Username,Password},From} -> 
            case maps:find(Username,Accounts) of 
                {ok, {Password,false}} -> 
                    From ! {?MODULE, {ok}},
                    loop(maps:update(Username,{Password,true},Accounts));
                {ok, {Password,true}} -> 
                    From ! {?MODULE, {error, login_status, "User already logged in"}},
                    loop(Accounts);
                _ -> 
                    From ! {?MODULE, {error, invalid_credentials, "Invalid credentials"}},
                    loop(Accounts)
            end;
        {{logout,Username},From} ->
            case maps:find(Username,Accounts) of 
                {ok, {Password,true}} -> 
                    From ! {?MODULE, ok},
                    loop(maps:update(Username,{Password,false},Accounts));
                _ -> 
                    From ! {?MODULE, {error, "User not logged in"}},
                    loop(Accounts)
            end
    end.