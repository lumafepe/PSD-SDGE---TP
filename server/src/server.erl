-module(server).
-export ([start/0]).

% Bind server to port
start() ->
    account_manager:start(),
    album_manager:start(),
    dht_manager:start(),
    {ok, LSock} = gen_tcp:listen(4321, [binary, {active, once}, {packet, 0}]),
    spawn(fun() -> acceptor(LSock) end),
    loop().

% Main loop to keep the process alive
loop() ->
    receive
        {interrupt, _Pid} ->
            io:format("Received SIGINT. Exiting.~n", []),
            ok;
        x ->
            io:format("Recieved ~p",[x]),
            loop()
    end.


% Process in a loop to accept new connections, create a process for each socket (client) that connects
acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    io:fwrite("\nConnected client socket: ~p.\n", [Sock]),
    spawn(fun() -> acceptor(LSock) end),
    gen_tcp:controlling_process(Sock, self()),
    authentication:authenticate(Sock).