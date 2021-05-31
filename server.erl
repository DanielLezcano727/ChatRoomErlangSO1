-module(server).
-define(Puerto, 1234).
-export([start/0, fin/1, receptor/2, worker/1, sockets_map/1, close/0]).

-define(MAX_NAMES, 30).
-define(MAX_LENGTH, 1000).
-define(MAX_CLIENTS, 2).

start()->
    {ok, Sock} = gen_tcp:listen(?Puerto, [ list, {active, false}]),
    register(cliente_handler, spawn(?MODULE,receptor, [Sock, 0])),
    register(map_handler, spawn(?MODULE, sockets_map, [maps:new()])),
    Sock.

fin(Sock) ->
    gen_tcp:close(Sock),
    ok.

accept_clients(Sock) -> 
    case gen_tcp:accept(Sock) of
        {ok, CSock}  ->
            io:format("LLegó un cliente~n"),
            spawn(?MODULE, worker, [CSock]),
            1;
        {error, closed} ->
            io:format("Se cerró el closed, nos vamos a mimir~n"),
            exit(normal);
        {error, Reason} ->
            io:format("Falló la espera del client por: ~p~n",[Reason]),
            0
    end.

close() ->
    map_handler ! {rip}.

receptor(Sock, CantClientes) ->
    if
        CantClientes < ?MAX_CLIENTS ->
            receptor(Sock, CantClientes + accept_clients(Sock));
        true ->
            receive
                exit -> receptor(Sock, CantClientes - 1)
            end
    end.
    
worker(Sock) ->
    gen_tcp:send(Sock, "Ingrese su nickname: "),
    ingresarNickname(Sock),
    receive
        {ok, Name} -> dedicatedListener(Sock, Name);
        {err} -> io:format("Ocurrio un error")
    end.

ingresarNickname(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Paq} ->
            Name = filtrar_ceros(Paq),
            case validationsNickname(Sock, Name) of
                ok -> 
                    map_handler ! {reg, Sock, Name},
                    self() ! {ok, Name},
                    gen_tcp:send(Sock, "OK\0");
                err ->
                    ingresarNickname(Sock)
            end;
        {error, closed} ->       % Este error creo que no lo estamos manejando 
            self() ! {err}
    end.

validation_length(Nick) ->
    if
        length(Nick) == 0 -> throw({err_empty});
        length(Nick) > ?MAX_NAMES -> throw({err_overf});
        true -> ok
    end.

validation_alpha(Nick) ->
    case re:run(Nick, "^[0-9A-Za-z-]+$") of
        nomatch -> throw({err_alpha});
        {match, _inf} -> ok
    end.

validation_used(Nick) ->
    map_handler ! {get, Nick, self()},
    receive
        available -> ok;
        _Other -> throw({err_used})
    end.

% Considerar hacer una funcion send con el sock y que esa se encargue de enviar cosas al cliente
% Verificar si es mejor directamente mandar el mensaje o devolverlo de la funcion
validationsNickname(Sock, Nick) ->
    try
        validation_length(Nick),
        validation_alpha(Nick),
        validation_used(Nick),
        ok
    catch
        {err_alpha} -> 
            gen_tcp:send(Sock, "Debe ser alfabetico\n\0"),
            err;
        {err_overf} -> 
            gen_tcp:send(Sock, "Excede el largo maximo\n\0"),
            err;
        {err_empty} -> 
            gen_tcp:send(Sock, "No debe ser vacio\n\0"),
            err;
        {err_used} ->
            gen_tcp:send(Sock, "Nickname usado\n\0"),
            err
    end.

dedicatedListener(Sock, Name) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Paquete} ->
            case packet_decoder(string:split(filtrar_ceros(Paquete), " "), Name, Sock) of
                "" -> ok;
                New -> dedicatedListener(Sock, New)
            end;
        {error, closed} ->
            io:format("El cliente cerró la conexion~n")
    end,
    exit(normal).
        
filtrar_ceros([0 | _Tl]) -> [];
filtrar_ceros([Hd | Tl]) -> [Hd] ++ filtrar_ceros(Tl).

packet_decoder(["/nickname", Rest], Name, Sock) ->
    case validationsNickname(Sock, Rest) of
        ok -> 
            map_handler ! {upd, Name, Sock, Rest},
            gen_tcp:send(Sock, "OK\0"),
            Rest;
        err -> Name
    end;
packet_decoder(["/msg", Rest], Name, Sock) ->
    case string:split(Rest, " ") of
        [""] ->
            gen_tcp:send(Sock, "Falta mensaje y destino\0");
        [_To, ""] ->
            gen_tcp:send(Sock, "Falta mensaje\0");
        [To, Msg] ->
            if
                length(Msg) > ?MAX_LENGTH ->
                    map_handler ! {get, To, self()},
                    receive
                        available -> gen_tcp:send(Sock, "Usuario inexistente\0");
                        SockTo -> gen_tcp:send(SockTo, [Name] ++ ": " ++ [Msg] ++ "\0")
                    end;
                true ->
                    gen_tcp:send(Sock, "Mensaje muy largo\0")
            end;
        [_To] ->
            gen_tcp:send(Sock, "Falta mensaje\0"),
        Name
    end;
packet_decoder(["/exit"], Name, _Sock) -> 
    map_handler ! {del, Name},
    cliente_handler ! exit,
    "";
packet_decoder(_Lista, Name, _Sock) -> 
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
            Pid ! maps:get(Nam, SocksMap, available),
            sockets_map(SocksMap);
        {del, Nam} ->
            NewMap = maps:remove(Nam, SocksMap),
            sockets_map(NewMap);
        {rip} ->
            lists:foreach(fun(Sock) -> gen_tcp:send(Sock, "EXIT\0") end, maps:values(SocksMap))
    end,
    exit(normal).