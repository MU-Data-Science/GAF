CC           = gcc
LD           = gcc
CFLAGS       = -Wall -Os -c
LDFLAGS      = -Wall -Os

ifdef AES192
CFLAGS += -DAES192=1
endif
ifdef AES256
CFLAGS += -DAES256=1
endif

default: client

client : aes.o client.o
	$(LD) $(LDFLAGS) -o $@ $^

client.o : client.c aes.h aes.o
	$(CC) $(CFLAGS) -o  $@ $<

aes.o : aes.c aes.h
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f *.o client