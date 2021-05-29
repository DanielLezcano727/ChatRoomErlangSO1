SERVIDOR = RemoteMultiThreadServer
CLIENTES = RemoteClient
FLAGS = -Wall -g -lpthread

ALL: server.o cliente.o

server.o: $(SERVIDOR).c
	gcc -o server $(SERVIDOR).c $(FLAGS)

cliente.o: $(CLIENTES).c
	gcc -o cliente $(CLIENTES).c $(FLAGS)

clean:
	rm server cliente
