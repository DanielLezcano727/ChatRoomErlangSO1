#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <signal.h>

/* Maxima cantidad de cliente que soportará nuestro servidor */
#define MAX_CLIENTS 25
#define MAX_NAMES 30
#define MAX_LENGTH 1024

/* Estructura de datos comunes para todos los hilos */
struct common {
  int spotsLeft; // requiere mutex
  pthread_mutex_t mutex;
  int sockets[MAX_CLIENTS]; // cada hilo se encarga del suyo solamente
  char *nicknames[MAX_CLIENTS]; // cada hilo se encarga del suyo solamente
};

/* Estructura que será el argumento para cada hilo */
typedef struct {
  int index;
  struct common *datosComunes;
} argumentos;

/* Anunciamos el prototipo del hijo */
void *child(void *arg);
/* Definimos una pequeña función auxiliar de error */
void error(char *msg);
/* Definimos una funcion para manejar las señales */
void error_handler(int arg);
/*
 * Si recibe parametros distinto de NULL, guarda las variables en variables internas.
 * Si recibe parametros iguales a NULL, avisa a los clientes que la conexion se va a interrumpir,
 * liberamos los recursos y ejecutamos la salida del servidor.
*/
void safe_close_connection(int *socks, char **nicks, int cant_socks);

int main(int argc, char **argv){
  /* Declaracion de variables */
  int sock, soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;
  struct common comun;

  /* Chequeo de parametros */
  if (argc <= 1) error("Faltan argumentos");

  /* Seteamos la funcion error_handler para la señal SIGINT */
  signal(SIGINT, error_handler);

  /* Creamos el socket */
  if( (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    error("Socket Init");

  /* Creamos a la dirección del servidor.*/
  servidor.sin_family = AF_INET; /* Internet */
  servidor.sin_addr.s_addr = INADDR_ANY; /**/
  servidor.sin_port = htons(atoi(argv[1]));

  /* Inicializamos el socket */
  if (bind(sock, (struct sockaddr *) &servidor, sizeof(servidor)))
    error("Error en el bind");

  printf("Binding successful, and listening on %s\n",argv[1]);

  /************************************************************/
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser *joinables* */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  /************************************************************/

  /* Inicializamos variables */
  comun.spotsLeft = MAX_CLIENTS;
  pthread_mutex_init(&comun.mutex, NULL);
  for(int i=0;i<MAX_CLIENTS;i++) {
    comun.nicknames[i] = NULL;
    comun.sockets[i] = -1;
  }

  /* Llamamos a la función de cierre seguro para preservar los datos necesario en una futura llamada por interrupción */
  safe_close_connection(comun.sockets, comun.nicknames, MAX_CLIENTS);

  /* Ya podemos aceptar conexiones */
  if(listen(sock, MAX_CLIENTS) == -1)
    error(" Listen error ");

  for(;;){ /* Comenzamos con el bucle infinito*/
    /* Now we can accept connections as they come*/
    clientelen = sizeof(clientedir);
    soclient = accept(sock, (struct sockaddr *) &clientedir, &clientelen);

    if (soclient == -1 || comun.spotsLeft == 0 ){
      send(soclient, "No se puedo aceptar la conexión.", sizeof("No se puedo aceptar la conexión."), 0);
    }else {
      int j;
      for(j = 0;comun.sockets[j] != -1; j++);
      comun.sockets[j] = soclient;
      argumentos args;
      args.datosComunes = &comun;
      args.index = j;
      pthread_create(&thread , NULL , child, (void *) &args);
      pthread_mutex_lock(&(comun.mutex));
      comun.spotsLeft--;
      pthread_mutex_unlock(&(comun.mutex));
    }
  }

  /* Código muerto */
  close(sock);
  return 0;
}

/*
 * Encargada de obtener un nickname disponible del usuario. Persiste hasta obtener un nickname disponible.
 */
void ingresar_nickname(int socket, char *nicknames[], char *buf) {
  int valid = 0;

  while(!valid) {
    valid = 1;

    send(socket, "Ingrese su nickname: ", sizeof("Ingrese su nickname: "), 0);
    recv(socket, buf, sizeof(char)*MAX_NAMES, 0);
    for(int i=0;i<MAX_CLIENTS;i++){
      if(nicknames[i] && strcmp(buf, nicknames[i]) == 0)
        valid = 0;
    }
  }
  send(socket, "OK", sizeof("OK"), 0);
}

/* Función que ejecutan los hilos del programa, atenderán las necesidades del lado del servidor de su cliente */
void * child(void *_arg){
  argumentos arg = *(argumentos*) _arg;
  char buf[MAX_LENGTH], rsp_mensaje[MAX_LENGTH], *temp;
  int *sockets = arg.datosComunes->sockets;
  char **nicknames = arg.datosComunes->nicknames;
  int i;

  ingresar_nickname(sockets[arg.index], nicknames, buf);

  nicknames[arg.index] = malloc(sizeof(char)*MAX_NAMES);
  strcpy(nicknames[arg.index], buf);

  recv(sockets[arg.index], buf, sizeof(buf), 0);
  while(strcmp(buf,"/exit")) {
    temp = strtok(buf, " ");

    if(!strcmp(temp,"/nickname")) {
      temp = strtok(NULL, "");
      if(temp) {
        strcpy(buf, temp);
        if (buf[0] == '\0' || buf[0] == '/' || strchr(buf, ' ') || strlen(buf) >= MAX_NAMES) {
          send(sockets[arg.index], "Nickname inválido, intente de nuevo", sizeof("Nickname inválido, intente de nuevo"), 0);
        }else {
          int valid = 1;
          for(int i=0;i<MAX_CLIENTS;i++){
            if(nicknames[i] && strcmp(buf, nicknames[i]) == 0)
            valid = 0;
          }
          if(valid) {
            strcpy(nicknames[arg.index], buf);
          }else {
            send(sockets[arg.index], "Nickname inválido, intente de nuevo", sizeof("Nickname inválido, intente de nuevo"), 0);
          }
        }
      }
    }else if(!strcmp(temp,"/msg")) {
      temp = strtok(NULL, " ");
      for(i = 0; i<MAX_CLIENTS && (!nicknames[i] || strcmp(nicknames[i], temp)); i++);
      if(i != MAX_CLIENTS) {
        strcpy(rsp_mensaje, strtok(NULL, ""));
        strcpy(buf, nicknames[arg.index]);
        strcat(buf, ": ");
        strcat(buf, rsp_mensaje);
        send(sockets[i], buf, sizeof(buf), 0);
      }else {
        strcpy(rsp_mensaje, temp);
        strcpy(buf, "No existe el usuario llamado ");
        strcat(buf, rsp_mensaje);
        send(sockets[arg.index], buf, sizeof(buf), 0);
      }
    }
    recv(sockets[arg.index], buf, sizeof(buf), 0);
  }
  printf("%s ha salido\n", nicknames[arg.index]);
  pthread_mutex_lock(&(arg.datosComunes->mutex));
  arg.datosComunes->spotsLeft++;
  pthread_mutex_unlock(&(arg.datosComunes->mutex));

  free(nicknames[arg.index]);
  nicknames[arg.index] = NULL;
  sockets[arg.index] = -1;
  return NULL;
}

void error(char *msg){
  exit((perror(msg), 1));
}

/*
 * En caso de una interrupción por parte del servidor se libera la memoria pendiente y se notifica a los usuarios
 * para que realicen su procedimiento de salida acorde.
 */
void safe_close_connection(int *socks, char **nicks, int cant_socks) {
  static int *sockets, cant;
  static char **nicknames;
  if(socks) {
    sockets = socks;
    cant = cant_socks;
    nicknames = nicks;
  } else if (sockets) {
    for(int i=0; i<cant; i++) {
      if (sockets[i] != -1) {
        send(sockets[i], "EXIT", sizeof("EXIT"), 0);
        close(sockets[i]);
      }
      if (nicknames[i] != NULL)
        free(nicknames[i]);
    }
  }
}

void error_handler(int arg) {
  safe_close_connection(NULL, NULL, 0);
  error("Ocurrio un error inesperado\n");
}
