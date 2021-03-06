-module(server).
-define(Puerto, 1234).
-export([start/0, fin/1, receptor/2, worker/1, sockets_map/2, close/0, show/0]).

-define(MAX_NAMES, 30).
-define(MAX_LENGTH, 1050).
-define(MAX_CLIENTS, 25).

% start inicia el servidor
start()->
    {ok, Sock} = gen_tcp:listen(?Puerto, [ list, {active, false}]),
    register(cliente_handler, spawn(?MODULE,receptor, [Sock, 0])),
    register(map_handler, spawn(?MODULE, sockets_map, [maps:new(), Sock])),
    Sock.

fin(Sock) ->
    gen_tcp:close(Sock),
    ok.

% accept_clients: Socket -> Nat
% crea el worker que atiende al cliente y retorna un entero para contar al cliente
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

% funcion dedicada para cerrar el programa
close() ->
    map_handler ! {rip}.

% receptor: Socket, Nat -> ...
% encargada de escuchar las conexiones entrantes y aceptarlas 
receptor(Sock, CantClientes) ->
    if
        CantClientes < ?MAX_CLIENTS ->
            receptor(Sock, CantClientes + accept_clients(Sock));
        true ->
            receive
                exit -> receptor(Sock, CantClientes - 1)
            end
    end.
    
% worker: Socket -> ...
% proceso dedicado a atender las peticiones de los clientes
worker(Sock) ->
    gen_tcp:send(Sock, "Ingrese su nickname: "),
    ingresarNickname(Sock),
    receive
        {ok, Name} -> dedicatedListener(Sock, Name);
        {err} -> 
            io:format("Ocurrio un error"),
            gen_tcp:close(Sock),
            cliente_handler ! exit
    end.

% ingresarNickname: Socket -> ...
% funcion dedicada para el ingreso del primer nickname del cliente
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
        {error, closed} -> 
            self() ! {err}
    end.

% validation_length: String -> Atom
% valida el largo para un nickname, si falla arroja error
validation_length(Nick) ->
    if
        length(Nick) == 0 -> throw({err_empty});
        length(Nick) > ?MAX_NAMES -> throw({err_overf});
        true -> ok
    end.

% validation_alpha: String -> Atom
% valida los caracteres para un nickname, si falla arroja error
validation_alpha(Nick) ->
    case re:run(Nick, "^[0-9A-Za-z-]+$") of
        nomatch -> throw({err_alpha});
        {match, _inf} -> ok
    end.

% validation_used: String -> Atom
% valida la disponibilidad para un nickname, si falla arroja error
validation_used(Nick) ->
    map_handler ! {get, Nick, self()},
    receive
        available -> ok;
        _Other -> throw({err_used})
    end.

% validationsNickname: Socket, String -> Atom
% ejecuta todas las validaciones para un nickname y devuelve el resultado
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

% Hilo encargado de escuchar los mensajes de los clientes.
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
        
% Dado una lista devuelve la lista concatenada sin ceros
filtrar_ceros([0 | _Tl]) -> [];
filtrar_ceros([Hd | Tl]) -> [Hd] ++ filtrar_ceros(Tl).

% Se encarga de ejecutar las acciones del cliente.
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
                length(Msg) < ?MAX_LENGTH ->
                    map_handler ! {get, To, self()},
                    receive
                        available -> gen_tcp:send(Sock, "Usuario inexistente\0");
                        SockTo -> gen_tcp:send(SockTo, [Name] ++ ": " ++ [Msg] ++ "\0")
                    end;
                true ->
                    gen_tcp:send(Sock, "Mensaje muy largo\0")
            end;
        [_To] ->
            gen_tcp:send(Sock, "Falta mensaje\0")
    end,
    Name;
packet_decoder(["/exit"], Name, _Sock) -> 
    map_handler ! {del, Name},
    cliente_handler ! exit,
    "";
packet_decoder(_Lista, Name, _Sock) -> 
    Name.

% sockets_map: Map, Socket -> ...
% Almacena los sockets de los clientes y los asocia con sus nicknames. 
% Acepta mensajes para registrar y actualizar nicknames, obtener sockets, borrar sockets
% y nicknames y cerrar las conexiones.
sockets_map(SocksMap, Sock) ->
    receive
        {reg, Sid, Nam} ->
            NewMap = maps:put(Nam, Sid, SocksMap),
            sockets_map(NewMap, Sock);
        {upd, Nam, Sid, NewNam} ->
            NewMap = maps:put(NewNam, Sid, maps:remove(Nam, SocksMap)),
            sockets_map(NewMap, Sock);
        {get, Nam, Pid} ->
            Pid ! maps:get(Nam, SocksMap, available),
            sockets_map(SocksMap, Sock);
        {del, Nam} ->
            NewMap = maps:remove(Nam, SocksMap),
            sockets_map(NewMap, Sock);
        {all} ->
            lists:foreach(fun(X) -> io:format("~p~n",[X]) end, maps:keys(SocksMap)),
            sockets_map(SocksMap, Sock);
        {rip} ->
            lists:foreach(fun(Socket) -> gen_tcp:send(Socket, "EXIT\0"), gen_tcp:close(Socket) end, maps:values(SocksMap)),
            gen_tcp:close(Sock)
    end,
    exit(normal).

% Funcion para debug. Muestra los nombres de los usuarios conectados.
show() ->
    map_handler ! {all}.