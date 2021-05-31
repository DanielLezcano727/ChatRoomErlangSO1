# Informe Ejercicio Semanal
# Alumnos: Bolzan Francisco - Lezcano Daniel

## Macros

 - MAX_NAMES: Longitud maxima de los nicknames.
 - MAX_LENGTH: Longitud maxima de los mensajes.
 - MAX_CLIENTS: Cantidad maxima aceptada de clientes.
 - Puerto: 1234

## Division en hilos

Decidimos implementar que al ejecutar el comando start inicie dos hilos:

 - cliente_handler: Es el encargado de escuchar nuevas conexiones con los clientes.
 - map_handler: Almacena los sockets de los clientes en un map, y los asocia con sus nicknames como clave. Es utilizado para efectuar el mensajeo entre los clientes y para cerrar las conexiones con los clientes en caso de cerrar el servidor.

cliente_handler al aceptar una conexion crea un hilo el cual se encargara de las peticiones del cliente.

## IPC

Los procesos que se encargan de la comunicacion con el cliente se comunican con map_handler a la hora de registrar y actualizar sus nicknames, obtener un socket dado un nickname y al borrar su socket de la lista y con cliente_handler a la hora de irse el cliente.

cliente_handler sumara a su estado la cantidad de clientes y en caso de llegar al maximo quedara a la espera de que alguno de los hilos encargados de los clientes le envie un mensaje exit.

A la hora de cerrar el servidor, map_handler cerrara los sockets de los clientes y el del servidor, causando el cierre de los hilos.

## Ejecucion del programa

### cliente

Para iniciar el cliente:

- make
- ./server PUERTO
- ./cliente DIRECCION_IP PUERTO_DEL_SV

Para cerrar el cliente puede utilizarse ctrl c.

### Servidor

Para iniciar el servidor:

- erl
- c(server).
- server:start().

Para cerrar el servidor:

 - server:close().

Funciones adicionales:

 - server:show(). (Muestra la lista de claves/nicknames registrados)