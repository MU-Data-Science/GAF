// This software is based on the GitHub repo tiny-AES-c (https://github.com/kokke/tiny-AES-c.git)
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>


// Enable ECB, CTR and CBC mode. Note this can be done before including aes.h or at compile-time.
// E.g. with GCC by using the -D flag: gcc -c aes.c -DCBC=0 -DCTR=1 -DECB=1
#define CBC 1
#define CTR 1
#define ECB 1

#include "aes.h"


static void phex(uint8_t* str);
static int xcrypt_message(uint8_t* input, size_t msg_len, uint8_t *key, uint8_t *iv);
static void generate_random_key(uint8_t *key, unsigned length);
static int parse_bytes(uint8_t *data, char *input_arg, uint16_t max_size);

#define MAXLEN 256
#define MAX_MSG_LEN 4096 * 8
#define MAX_KEY_SIZE 128
#define HEADER_SIZE 8
#define IV_LENGTH 16


int main(int argc, char *argv[])
{
#if defined(AES256)
    printf("\nTesting AES256\n\n");
#elif defined(AES192)
    printf("\nTesting AES192\n\n");
#elif defined(AES128)
    printf("\nTesting AES128\n\n");
#else
    printf("You need to specify a symbol between AES128, AES192 or AES256. Exiting");
    return 0;
#endif

    if (argc < 5) {
        printf("Usage: test <inputfile> <outputfile> <operation: en | de> <key>\n");
        exit(1);
    }

    char input[MAXLEN], output[MAXLEN], operation[MAXLEN], argkey[MAXLEN];
    char read_buffer[MAX_MSG_LEN];

    strcpy(input, argv[1]);    
    strcpy(output, argv[2]);
    strcpy(operation, argv[3]);
    strcpy(argkey, argv[4]);

    FILE *fpin = fopen(input, "rb");
    FILE *fpout = fopen(output, "wb");

    assert(fpin);
    assert(fpout);

    size_t n, n_iv;

    uint8_t cipher_key[MAX_KEY_SIZE], iv[IV_LENGTH];

    int m = parse_bytes(cipher_key, argkey, MAX_KEY_SIZE);
    printf("Key bytes: %d\n", m);
    phex(cipher_key);

    while (!feof(fpin)) {
        if (!strcmp(operation, "en")) {
            n = fread(read_buffer, 1, MAX_MSG_LEN, fpin);
            if (n == 0) continue;

            // Generate random IV
            generate_random_key(iv, IV_LENGTH);
            //phex(iv);

            // Encrypt the message
            xcrypt_message((uint8_t *) read_buffer, n, cipher_key, iv);
            
            // Write to output file
            size_t cipher_len = n;
            size_t num_bytes_header = fwrite((void *) &cipher_len, 1, sizeof(cipher_len), fpout);

            // Write the IV
            size_t num_bytes_iv = fwrite((void *) iv, 1, IV_LENGTH, fpout);

            // Write the message
			size_t num_bytes_payload = fwrite(read_buffer, 1, cipher_len, fpout);

            //printf("Size header: %ld, size payload: %ld, size IV: %ld\n", num_bytes_header, num_bytes_payload, num_bytes_iv);
        }
        else if (!strcmp(operation, "de")) {
			size_t cipher_size;
			n = fread(&cipher_size, 1, HEADER_SIZE, fpin);
			if (n == 0) continue;

            // Read the IV
            n_iv = fread(iv, 1, IV_LENGTH, fpin);
			if (n_iv == 0) continue;

            // Read the message
			n = fread(read_buffer, 1, cipher_size, fpin);
			if (n == 0) continue;

            //phex(iv);
            //phex(cipher_key);

            xcrypt_message((uint8_t *) read_buffer, n, cipher_key, iv);
            size_t num_bytes_payload = fwrite(read_buffer, 1, n, fpout);
        }
        else {
            printf("Unsupported operation\n");
            break;
        }
    }

    fclose(fpin);
    fclose(fpout);
    return(EXIT_SUCCESS);
}


// prints string as hex
static void phex(uint8_t* str)
{

#if defined(AES256)
    uint8_t len = 32;
#elif defined(AES192)
    uint8_t len = 24;
#elif defined(AES128)
    uint8_t len = 16;
#endif

    unsigned char i;
    for (i = 0; i < len; ++i)
        printf("%.2x", str[i]);
    printf("\n");
}

/** Parse bytes from command line argument */
static int
parse_bytes(uint8_t *data, char *input_arg, uint16_t max_size)
{
	unsigned byte_count;
	char *token;

	errno = 0;
	for (byte_count = 0, token = strtok(input_arg, ":");
			(byte_count < max_size) && (token != NULL);
			token = strtok(NULL, ":")) {

		int number = (int)strtol(token, NULL, 16);

		if (errno == EINVAL || errno == ERANGE || number > 0xFF)
			return -1;

		data[byte_count++] = (uint8_t)number;
	}

	return byte_count;
}


/** Generate random key (from DPDK codebase) */
static void
generate_random_key(uint8_t *key, unsigned length)
{
	int fd;
	int ret;

	fd = open("/dev/urandom", O_RDONLY);
	if (fd < 0)
		exit(EXIT_FAILURE);

	ret = read(fd, key, length);
	close(fd);

	if (ret != (signed)length)
		exit(EXIT_FAILURE);
}

static int xcrypt_message(uint8_t *input, size_t msg_len, uint8_t *key, uint8_t *iv)
{
    struct AES_ctx ctx;
    
    AES_init_ctx_iv(&ctx, key, iv);
    AES_CTR_xcrypt_buffer(&ctx, input, msg_len);
    
    return(0);
}