CLIENTES = RemoteClient
FLAGS = -Wall -g -lpthread

ALL: cliente.o

cliente.o: $(CLIENTES).c
	gcc -o cliente $(CLIENTES).c $(FLAGS)

clean:
	rm cliente *.beam
