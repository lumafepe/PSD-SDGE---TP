-module(answer).

-export([respond/3]).

% reply to request
respond(Socket,Result,Msg) ->
    Reply = messages:encode_msg(#{type=>'REPLY', reply => #{result=>Result, message=>Msg}}, 'Message'),
    gen_tcp:send(Socket,Reply).