-module(device_server).

-compile(export_all).



start() ->
    device_state:start(),
	{ok, Listen} = gen_tcp:listen(51, [binary,
					    {reuseaddr, true},
					    {active, true}]),
	spawn(?MODULE,controller,[Listen]).
controller(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).
loop(Socket) ->
	receive
	    {tcp, Socket, Command} ->
			case Command of
                <<"checked\r\n">> ->
                    State = proplists:get_value(input,device_state:state()),
					gen_tcp:send(Socket,list_to_binary(State));
                <<"output\r\n">> ->
                    State = proplists:get_value(output,device_state:state()),
                    gen_tcp:send(Socket,list_to_binary(State))
            end,
            loop(Socket);
	    {tcp_closed, Socket} ->
            gen_tcp:close(Socket),
            io:format("Server socket ~p closed~n",[Socket])
	end.
