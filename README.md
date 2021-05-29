# Informe Ejercicio Semanal
# Alumnos: Bolzan Francisco - Lezcano Daniel

## Decisiones

### Comunicación

La comunicación que decidimos implementar luego de la consulta del martes 11 de mayo fue en la que cada usuario puede comunicar a otro a través de
`/msg nickname text` sin oportunidad de emitir mensajes broadcast.

### División de los trabajos del cliente

Decidimos crear un hilo con `pthread_create`, el cual se encargue de leer lo que se reciba del servidor, y el programa principal que se encargue de leer la entrada del cliente y enviarla al servidor.

### División de los trabajos del servidor

El servidor acepta hasta MAX_CLIENTS clientes (constante definida en el archivo del servidor) y para cada cliente se crea un nuevo hilo que se encargue de su respectiva comunicacion con el cliente. El programa principal estara a la espera de nuevas conexiones y les asignara un hilo a las entrantes.

## Mecanismo de cierre

El mecanismo de cierre que implementamos tanto del lado del servidor como del cliente es la captura de la señal SIGINT.

 - En el caso del cliente, al finalizar con esta señal le enviara al servidor un mensaje de exit y saldra del programa
 - En el caso del servidor, le enviara a cada uno de los clientes un mensaje de finalizacion, para los cuales los clientes deberan finalizar el programa

## Ejecucion del programa

- make
- ./server PUERTO
- ./cliente DIRECCION_IP PUERTO_DEL_SV
