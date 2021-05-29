#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>

/*
  Ejecución:
  - make
  - ./server 5000
  (en otra terminal)- ./cliente 127.0.0.1 5000
*/

#define MAX_NAMES 30
#define MAX_LENGTH 1024

void error(char *msg) {
  exit((perror(msg), 1));
}

/* Recibe las señales y ejecuta el cierre de la conexion con los clientes */
void error_handler(int arg);
/*
 * Es la encargada de liberar recursos y comunica al servidor que puede liberar los recursos del cliente.
 * Si recibe un argumento distinto de NULL, los guarda en variables internas
 * Si recibe NULL y las variables internas son distintas de NULL, envia al servidor el mensaje de salida.
 */
void safe_close_connection(int *sock);

/*
 * Encargada de escuchar las comunicaciones que llegan al cliente desde el servidor.
 * Dichos mensajes ya llegan "procesados" por lo que solo se los recibe y comunica al usuario de ser necesario.
 */
void * listener(void *_arg);

/*
 * Encargada de enviar los mensajes del cliente al servidor una vez establecida la conexión y el nickname inicial.
 */
void sender(int socket);

/*
 * Lee del socket que recibe como parametro y lo guarda en el bufer tambien recibido como parametro.
 * En caso de recibir un mensaje de error del servidor ejecuta el proceso de cierre del cliente.
 */
void read_from_sv(int sock, char *buf, int max);

/*
 * Encargada de procesar el input del usuario para su correcto uso posterior.
 * De superarse el tamaño máximo se desecha la lectura y se limpia el buffer del stdin.
 */
int read_input(char *dest, int max);

/*
 * Dado un socket envia el nickname ingresado por el usuario hasta que sea valido.
*/
void ingresar_nickname(int sock);

int main(int argc, char **argv){
  /* Declaracion de variables */
  int sock;
  pthread_t thread;
  pthread_attr_t attr;
  struct addrinfo *resultado;

  /* Redefinimos el comportamiento de las señales */
  signal(SIGINT, error_handler);

  /*Chequeamos mínimamente que los argumentos fueron pasados*/
  if(argc != 3){
    fprintf(stderr,"El uso es \'%s IP port\'", argv[0]);
    exit(1);
  }

  /* Inicializamos el socket */
  if( (sock = socket(AF_INET , SOCK_STREAM, 0)) < 0 )
    error("No se pudo iniciar el socket");

  /* Buscamos la dirección del hostname:port */
  if (getaddrinfo(argv[1], argv[2], NULL, &resultado)){
    fprintf(stderr,"No se encontro el host: %s \n",argv[1]);
    exit(2);
  }

  /* llamamos a la función de salida segura para preservar la dirección del socket a cerrar en caso de interrupción */
  safe_close_connection(&sock);

  /* Intentamos establecer la conexion con el servidor */
  if(connect(sock, (struct sockaddr *) resultado->ai_addr, resultado->ai_addrlen) != 0)
    error("No se pudo conectar :(. ");

  /* Argumentos de los hilos */
  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);

  ingresar_nickname(sock);
  /* Recibimos lo que nos manda el servidor */
  pthread_create(&thread , NULL , listener, (void *) &sock);

  sender(sock);

  freeaddrinfo(resultado);
  close(sock);
  return 0;
}

void ingresar_nickname(int sock) {
  char buf[MAX_NAMES] = "";
  int overf, no_valid = 1;

  recv(sock, buf, sizeof(buf),0);
  printf("%s", buf);
  while(no_valid) {
    overf = !read_input(buf, MAX_NAMES);

    if (buf[0] == '\0' || buf[0] == '/' || strchr(buf, ' ') || overf)
      printf("Nickname invalido.\nIngrese un nickname: ");
    else {
      send(sock, buf, sizeof(buf), 0);
      read_from_sv(sock, buf, MAX_NAMES);
      no_valid = strcmp("OK", buf);
      if(no_valid)
        printf("%s", buf);
    }
  }
}

void sender(int sock) {
  char buf[MAX_LENGTH] = "";
  while(strcmp(buf, "/exit")) {
    if(read_input(buf, MAX_LENGTH)){
      send(sock, buf, sizeof(buf),0);

    }else{
      printf("Mensaje muy largo\n");
    }
  }
}

void *listener(void *_arg){
  int sock = *(int*) _arg;
  char buf[MAX_LENGTH];

  while(1) {
    read_from_sv(sock, buf, MAX_LENGTH);
    printf("%s\n", buf);
  }

  return NULL;
}

int read_input(char *dest, int max){
  char ch;
  int overf;

  fgets(dest, max, stdin);

  if ((overf = dest[strlen(dest)-1] != '\n'))
    while(((ch = getchar()!='\n') && (ch!=EOF)));
  else
    dest[strlen(dest)-1]='\0';

  return !overf;
}

void error_handler(int arg) {
  safe_close_connection(NULL);
  error("Ocurrio un error inesperado\n");
}

void safe_close_connection(int *sock){
  static int socket;

  if(sock)
    socket = *sock;
  else if(socket) {
    send(socket, "/exit", sizeof("/exit"),0);
    close(socket);
  }

}

void read_from_sv(int sock, char *buf, int max) {
  recv(sock, buf, sizeof(char)*max, 0);
  if (!strcmp("EXIT", buf)) {
    close(sock);
    error("Ocurrio un error inesperado\n");
  }
}
