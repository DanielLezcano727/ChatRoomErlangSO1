-module(echoServ).
-define(Puerto, 1234).
-export([start/0, fin/1, receptor/1, worker/1, sockets_map/1]).

start()->
    %% register(servidor, self()),
    {ok, Sock} = gen_tcp:listen(?Puerto
                                , [ list, {active, false}]),
    spawn(?MODULE,receptor, [Sock]),
    register(map_handler, spawn(?MODULE, sockets_map, [maps:new()])),
    Sock.

fin(Sock) ->
    gen_tcp:close(Sock),
    ok.

receptor(Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, CSock}  ->
            io:format("LLegó un cliente~n"),
            spawn(?MODULE, worker, [CSock]);
        {error, closed} ->
            io:format("Se cerró el closed, nos vamos a mimir~n"),
            exit(normal);
        {error, Reason} ->
            io:format("Falló la espera del client por: ~p~n",[Reason])
    end,
    receptor(Sock).
    
worker(Sock) ->
    ingresarNickname(Sock),
    receive
        {ok, Name} -> dedicatedListener(Sock, Name);
        {err} -> io:format("Ocurrio un error")
    end.

ingresarNickname(Sock) ->
    gen_tcp:send(Sock, "Ingrese su nickname: "),
    case gen_tcp:recv(Sock, 0) of
        {ok, Paq} ->
            Name = filtrar_ceros(Paq),
            map_handler ! {reg, Sock, Name},
            self() ! {ok, Name},
            gen_tcp:send(Sock, "OK\0");
            {error, closed} ->
                self() ! {err}
    end.
        
dedicatedListener(Sock, Name) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Paquete} ->
            Temp = packet_decoder(string:split(filtrar_ceros(Paquete), " "), Name, Sock),
            if 
                Temp /= "" ->
                    dedicatedListener(Sock, Temp);
                true ->
                    done
            end;
        {error, closed} ->
            io:format("El cliente cerró la conexión~n")
end.
        
filtrar_ceros([0 | _Tl]) -> [];
filtrar_ceros([Hd | Tl]) -> [Hd] ++ filtrar_ceros(Tl).

packet_decoder(["/nickname", Rest], Name, Sock) ->
    map_handler ! {upd, Name, Sock, Rest},
    gen_tcp:send(Sock, "OK\0"),
    Rest;
packet_decoder(["/msg", Rest], Name, _Sock) ->
    [To, Msg] = string:split(Rest, " "),
    map_handler ! {get, To, self()},
    receive
        SockTo -> gen_tcp:send(SockTo, [Name] ++ ": " ++ [Msg] ++ "\0")
    end,
    Name;
packet_decoder(["/exit"], Name, _Sock) -> 
    map_handler ! {del, Name},
    "";
packet_decoder(_Lista, Name, _Sock) -> 
    %io:format("Basura~n"),
    Name.

sockets_map(SocksMap) ->
    receive
        {reg, Sid, Nam} ->
            NewMap = maps:put(Nam, Sid, SocksMap),
            sockets_map(NewMap);
        {upd, Nam, Sid, NewNam} ->
            NewMap = maps:put(NewNam, Sid, maps:remove(Nam, SocksMap)),
            sockets_map(NewMap);
        {get, Nam, Pid} ->
            Pid ! maps:get(Nam, SocksMap),
            sockets_map(SocksMap);
        {del, Nam} ->
            NewMap = maps:remove(Nam, SocksMap),
            sockets_map(NewMap)
    end.