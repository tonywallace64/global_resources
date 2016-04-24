-module(checker).
-export ([main/1]).

main(_) ->
    {ok,Pid} = globals:start_link(),
    Tcp_ports = gen_server:call(Pid,{lookup,{tcp_port,'_','_'}}),
    io:format("tcp_port lookup result: ~p~n",[Tcp_ports]),
    check_ports(Tcp_ports,Pid,gb_sets:empty()),
    file:write_file("config_checked_okay", <<>> ).

check_ports([],_,Acc) ->
    Acc;
check_ports([X={tcp_port,Port,{Client,Module}}|T],Svr,Acc) ->
    io:format("checking ~p",[X]),
    NewSet = gb_sets:insert(Port,Acc),
    [{client,Client,_}] = gen_server:call(Svr,{lookup,{client,Client,'_'}}),
    [{module,Module,_}] = gen_server:call(Svr,{lookup,{module,Module,'_'}}),
    io:format("passed~n"),
    check_ports(T,Svr,NewSet).
