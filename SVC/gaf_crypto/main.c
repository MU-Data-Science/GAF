/* SPDX-License-Identifier: BSD-3-Clause
 * Copyright(c) 2010-2015 Intel Corporation
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <getopt.h>
#include <signal.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <rte_string_fns.h>
#include <rte_branch_prediction.h>
#include <rte_common.h>
#include <rte_cryptodev.h>
#include <rte_cycles.h>
#include <rte_debug.h>
#include <rte_eal.h>
#include <rte_ether.h>
#include <rte_ethdev.h>
#include <rte_interrupts.h>
#include <rte_ip.h>
#include <rte_launch.h>
#include <rte_lcore.h>
#include <rte_log.h>
#include <rte_malloc.h>
#include <rte_mbuf.h>
#include <rte_memcpy.h>
#include <rte_memory.h>
#include <rte_mempool.h>
#include <rte_per_lcore.h>
#include <rte_prefetch.h>
#include <rte_random.h>
#include <rte_hexdump.h>

#define RX_RING_SIZE 1024
#define TX_RING_SIZE 1024

#define NUM_MBUFS 8191
#define MAX_BUF_SIZE 4096 * 8
#define MBUF_CACHE_SIZE 250
#define BURST_SIZE 32
#define MAX_STR_LEN 128
#define MAX_MSG_LEN 4096
#define MAX_LINE_LEN 128
#define MAX_KEY_SIZE 128
#define MAX_IV_SIZE 16
#define MAXIMUM_IV_LENGTH	16
#define HEADER_SIZE 8
#define MAX_COMMANDS 4
#define MAX_PORTS 2
#define MAX_RX_QUEUE_PER_LCORE 16

#define IV_OFFSET 	(sizeof(struct rte_crypto_op) + \
				sizeof(struct rte_crypto_sym_op))


/* Global signal */
static volatile bool signal_received;

static const char short_options[] =
	"O:"  /* output filename */
	"I:"   /* input filename */
	"d:"  /* destination MAC address */
	"s:"  /* sender MAC address */
	"a:"  /* cipher algorithm */
	"o:"  /* cipher operation */
	"k:"  /* cipher key */
	"v:"  /* cipher iv */
	"p:"  /* named pipe */
	"m:" /* number of commands */
	;

struct gaf_crypto_iv {
	uint8_t *data;
	uint16_t length;
};

struct rte_mempool *gaf_crypto_op_pool;
struct rte_mempool *gaf_crypto_mbuf_pool;

static struct {
	struct rte_mempool *sess_mp;
} session_pool_socket[RTE_MAX_NUMA_NODES];

enum cdev_type {
	CDEV_TYPE_SW
};

enum gaf_crypto_xform_chain {
	GAF_CRYPTO_CIPHER_ONLY,
	GAF_CRYPTO_CIPHER_HASH
};

struct gaf_crypto_options {
	char input_file[MAX_STR_LEN], output_file[MAX_STR_LEN], named_pipe[MAX_STR_LEN];
	struct rte_ether_addr dst_eth_addr;
	struct rte_ether_addr src_eth_addr;
	unsigned nb_ports_per_lcore;

	enum cdev_type type;

	enum gaf_crypto_xform_chain xform_chain; // GAF_CRYPTO_CIPHER_ONLY

	struct rte_crypto_sym_xform cipher_xform;
	uint8_t cipher_key[MAX_KEY_SIZE];
	unsigned ckey_param;

	uint8_t do_cipher;
	struct gaf_crypto_iv cipher_iv; // Initialized later in the code
	unsigned int cipher_iv_param; // = 0;
	int cipher_iv_random_size; // = -1;

	uint64_t cryptodev_mask; // = UINT64_MAX;

	uint16_t block_size;
	char string_type[MAX_STR_LEN]; // = SW

	int num_commands;
};

/** gaf crypto lcore params */
struct gaf_crypto_params {
	uint8_t dev_id;
	uint8_t qp_id;

	unsigned block_size;

	uint32_t cipher_dataunit_len;

	struct gaf_crypto_iv cipher_iv;
	void *session;

	uint8_t do_cipher;

	enum rte_crypto_cipher_algorithm cipher_algo;
};

struct pkt_buffer {
	unsigned len;
	struct rte_mbuf *buffer[BURST_SIZE];
};

struct op_buffer {
	unsigned len;
	struct rte_crypto_op *buffer[BURST_SIZE];
};

/** lcore configuration */
struct lcore_queue_conf {
	unsigned nb_rx_ports;
	uint16_t rx_port_list[MAX_RX_QUEUE_PER_LCORE];

	unsigned nb_crypto_devs;
	unsigned cryptodev_list[MAX_RX_QUEUE_PER_LCORE];

	struct op_buffer op_buf[RTE_CRYPTO_MAX_DEVS];
	struct pkt_buffer pkt_buf[RTE_MAX_ETHPORTS];
} __rte_cache_aligned;

struct lcore_queue_conf lcore_queue_conf[RTE_MAX_LCORE];

/* Per-port statistics struct */
struct gaf_port_statistics {
	uint64_t tx;
	uint64_t rx;

	uint64_t crypto_enqueued;
	uint64_t crypto_dequeued;

	uint64_t dropped;
} __rte_cache_aligned;

struct gaf_crypto_statistics {
	uint64_t enqueued;
	uint64_t dequeued;

	uint64_t errors;
} __rte_cache_aligned;

struct gaf_port_statistics port_statistics[RTE_MAX_ETHPORTS];
struct gaf_crypto_statistics crypto_statistics[RTE_CRYPTO_MAX_DEVS];

static void
display_cipher_info(struct gaf_crypto_options *options)
{
	printf("\n---- Cipher information ---\n");
	printf("Algorithm: %s\n",
		rte_cryptodev_get_cipher_algo_string(options->cipher_xform.cipher.algo));
	rte_hexdump(stdout, "Cipher key:",
			options->cipher_xform.cipher.key.data,
			options->cipher_xform.cipher.key.length);
	rte_hexdump(stdout, "IV:", options->cipher_iv.data, options->cipher_iv.length);
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

/* Check if device has to be HW/SW or any */
static int
check_type(const struct gaf_crypto_options *options,
		const struct rte_cryptodev_info *dev_info)
{
	if (options->type == CDEV_TYPE_SW &&
			!(dev_info->feature_flags & RTE_CRYPTODEV_FF_HW_ACCELERATED))
		return 0;

	return -1;
}

static inline int
check_supported_size(uint16_t length, uint16_t min, uint16_t max,
		uint16_t increment)
{
	uint16_t supp_size;

	/* Single value */
	if (increment == 0) {
		if (length == min)
			return 0;
		else
			return -1;
	}

	/* Range of values */
	for (supp_size = min; supp_size <= max; supp_size += increment) {
		if (length == supp_size)
			return 0;
	}

	return -1;
}

static int
check_iv_param(const struct rte_crypto_param_range *iv_range_size,
		unsigned int iv_param, int iv_random_size,
		uint16_t iv_length)
{
	/*
	 * Check if length of provided IV is supported
	 * by the algorithm chosen.
	 */
	if (iv_param) {
		if (check_supported_size(iv_length,
				iv_range_size->min,
				iv_range_size->max,
				iv_range_size->increment)
					!= 0)
			return -1;
	/*
	 * Check if length of IV to be randomly generated
	 * is supported by the algorithm chosen.
	 */
	} else if (iv_random_size != -1) {
		if (check_supported_size(iv_random_size,
				iv_range_size->min,
				iv_range_size->max,
				iv_range_size->increment)
					!= 0)
			return -1;
	}

	return 0;
}

/** Parse crypto cipher algo option command line argument */
static int
parse_cipher_algo(char *optarg, enum rte_crypto_cipher_algorithm *algo)
{	
	if (rte_cryptodev_get_cipher_algo_enum(algo, optarg) < 0) {
		RTE_LOG(ERR, USER1, "Cipher algorithm specified "
				"not supported!\n");
		return -1;
	}

	return 0;
}

/** Parse crypto cipher operation command line argument */
static int
parse_cipher_op(char *optarg, enum rte_crypto_cipher_operation *op)
{
	if (strcmp("ENCRYPT", optarg) == 0) {
		*op = RTE_CRYPTO_CIPHER_OP_ENCRYPT;
		return 0;
	} else if (strcmp("DECRYPT", optarg) == 0) {
		*op = RTE_CRYPTO_CIPHER_OP_DECRYPT;
		return 0;
	}

	printf("Cipher operation not supported!\n");
	return -1;
}

/** Parse crypto key  command line argument */
static int
parse_cipher_key(char *optarg, struct gaf_crypto_options *op)
{
	op->ckey_param = 1;
	op->cipher_xform.cipher.key.length = parse_bytes(op->cipher_key, optarg, MAX_KEY_SIZE);
	if (op->cipher_xform.cipher.key.length > 0)
		return 0;
	else {
		printf("Cipher key length not supported\n");
		return -1;
	}
}

/** Parse crypto key  command line argument */
static int
parse_cipher_iv(char *optarg, struct gaf_crypto_options *op)
{
	op->cipher_iv_param = 1;
	op->cipher_iv.length = parse_bytes(op->cipher_iv.data, optarg, MAX_IV_SIZE);
	if (op->cipher_iv.length > 0)
		return 0;
	else {
		printf("Cipher IV length not supported\n");
		return -1;
	}
}

/*
 * Initializes a given port using global settings and with the RX buffers
 * coming from the mbuf_pool passed as a parameter.
 */

/* Main functional part of port initialization. 8< */
static inline int
port_init(uint16_t port, struct rte_mempool *mbuf_pool)
{
	struct rte_eth_conf port_conf;
	const uint16_t rx_rings = 1, tx_rings = 1;
	uint16_t nb_rxd = RX_RING_SIZE;
	uint16_t nb_txd = TX_RING_SIZE;
	int retval;
	uint16_t q;
	struct rte_eth_dev_info dev_info;
	struct rte_eth_txconf txconf;

	if (!rte_eth_dev_is_valid_port(port))
		return -1;

	memset(&port_conf, 0, sizeof(struct rte_eth_conf));

	retval = rte_eth_dev_info_get(port, &dev_info);
	if (retval != 0) {
		printf("Error during getting device (port %u) info: %s\n",
				port, strerror(-retval));
		return retval;
	}

	if (dev_info.tx_offload_capa & RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE)
		port_conf.txmode.offloads |=
			RTE_ETH_TX_OFFLOAD_MBUF_FAST_FREE;

	/* Configure the Ethernet device. */
	retval = rte_eth_dev_configure(port, rx_rings, tx_rings, &port_conf);
	if (retval != 0)
		return retval;

	retval = rte_eth_dev_adjust_nb_rx_tx_desc(port, &nb_rxd, &nb_txd);
	if (retval != 0)
		return retval;

	/* Allocate and set up 1 RX queue per Ethernet port. */
	for (q = 0; q < rx_rings; q++) {
		retval = rte_eth_rx_queue_setup(port, q, nb_rxd,
				rte_eth_dev_socket_id(port), NULL, mbuf_pool);
		if (retval < 0)
			return retval;
	}

	txconf = dev_info.default_txconf;
	txconf.offloads = port_conf.txmode.offloads;
	/* Allocate and set up 1 TX queue per Ethernet port. */
	for (q = 0; q < tx_rings; q++) {
		retval = rte_eth_tx_queue_setup(port, q, nb_txd,
				rte_eth_dev_socket_id(port), &txconf);
		if (retval < 0)
			return retval;
	}

	/* Starting Ethernet port. 8< */
	retval = rte_eth_dev_start(port);
	/* >8 End of starting of ethernet port. */
	if (retval < 0)
		return retval;

	/* Display the port MAC address. */
	struct rte_ether_addr addr;
	retval = rte_eth_macaddr_get(port, &addr);
	if (retval != 0)
		return retval;

	printf("Port %u MAC: %02" PRIx8 " %02" PRIx8 " %02" PRIx8
			   " %02" PRIx8 " %02" PRIx8 " %02" PRIx8 "\n",
			port, RTE_ETHER_ADDR_BYTES(&addr));

	/* Enable RX in promiscuous mode for the Ethernet device. */
	retval = rte_eth_promiscuous_enable(port);
	/* End of setting RX port in promiscuous mode. */
	if (retval != 0)
		return retval;

	return 0;
}
/* >8 End of main functional part of port initialization. */

/*
 * The lcore main. This is the main thread that does the work, reading from
 * an input port and writing to an output port.
 */

 /* Basic forwarding application lcore. 8< */
static int
lcore_main(__rte_unused void *arg)
{
	uint16_t port;

	/*
	 * Check that the port is on the same NUMA node as the polling thread
	 * for best performance.
	 */
	RTE_ETH_FOREACH_DEV(port)
		if (rte_eth_dev_socket_id(port) >= 0 &&
				rte_eth_dev_socket_id(port) !=
						(int)rte_socket_id())
			printf("WARNING, port %u is on remote NUMA node to "
					"polling thread.\n\tPerformance will "
					"not be optimal.\n", port);

	printf("\nCore %u forwarding packets. [Ctrl+C to quit]\n",
			rte_lcore_id());

	/* Main work of application loop. 8< */
	for (;;) {
		/*
		 * Receive packets on a port and forward them on the paired
		 * port. The mapping is 0 -> 1, 1 -> 0, 2 -> 3, 3 -> 2, etc.
		 */
		RTE_ETH_FOREACH_DEV(port) {

			/* Get burst of RX packets, from first port of pair. */
			struct rte_mbuf *bufs[BURST_SIZE];
			const uint16_t nb_rx = rte_eth_rx_burst(port, 0,
					bufs, BURST_SIZE);

			if (unlikely(nb_rx == 0))
				continue;

			printf("Port received: %u, forwarding packets: %d %d\n", port, nb_rx, bufs[0]->data_len);

			/* Send burst of TX packets, to second port of pair. */
			const uint16_t nb_tx = rte_eth_tx_burst(port ^ 1, 0,
					bufs, nb_rx);

			/* Free any unsent packets. */
			if (unlikely(nb_tx < nb_rx)) {
				uint16_t buf;
				for (buf = nb_tx; buf < nb_rx; buf++)
					rte_pktmbuf_free(bufs[buf]);
			}
		}
	}
	/* >8 End of loop. */
	return 0;
}
/* >8 End Basic forwarding application lcore. */

/* Launch a function on lcore. 8< */
static int
lcore_hello(__rte_unused void *arg)
{
	unsigned lcore_id;
	lcore_id = rte_lcore_id();
	printf("Hello Praveen from core %u\n", lcore_id);
	return 0;
}

/* Display command line arguments usage */
static void
gaf_crypto_usage(const char *prgname)
{
	printf("%s [EAL options] --\n"
		"  -f INPUT: an input file to transmit\n"
		"  -d DST_ETH_ADDR: MAC address of the receiver (XX:XX:XX:XX:XX:XX) \n"
		"  -s SRC_ETH_ADDR: MAC address of the sender (XX:XX:XX:XX:XX:XX)\n"
		"  -a ALGO: Cipher algorithm (e.g., aes-cbc)\n"
		"  -o ENCRYPT | DECRYPT\n"
		"  -k KEY: Cipher key (bytes separated with \":\")\n",
		   prgname);
}

/* Parse the argument given in the command line of the application */
static int
gaf_crypto_parse_args(int argc, char **argv, struct gaf_crypto_options *options)
{
	int opt, ret;
	char **argvopt;
	char *prgname = argv[0];

	argvopt = argv;

	while ((opt = getopt(argc, argvopt, short_options)) != EOF) {

		switch (opt) {
		/* input file */
		case 'I':
			strcpy(options->input_file, optarg);
			break;
		/* output file */
		case 'O':
			strcpy(options->output_file, optarg);
			break;
		/* destination MAC address */	
		case 'd':
			ret = rte_ether_unformat_addr(optarg, &options->dst_eth_addr);
			if (ret != 0) { 
				printf("Invalid destination MAC address\n");
				return -1;
			}
			break;
		/* sender MAC address */	
		case 's':
			ret = rte_ether_unformat_addr(optarg, &options->src_eth_addr);
			if (ret != 0) { 
				printf("Invalid sender MAC address\n");
				return -1;
			}
			break;
		/* cipher algorithm */	
		case 'a':
			ret = parse_cipher_algo(optarg, &options->cipher_xform.cipher.algo);
			if (ret != 0) { 
				printf("Invalid cipher algorithm\n");
				return -1;
			}
			break;
		/* cipher operation */	
		case 'o':
			ret = parse_cipher_op(optarg, &options->cipher_xform.cipher.op);
			if (ret != 0) { 
				printf("Invalid cipher operation\n");
				return -1;
			}
			break;
		/* cipher key */	
		case 'k':
			ret = parse_cipher_key(optarg, options);
			if (ret != 0) { 
				printf("Invalid cipher key\n");
				return -1;
			}
			break;
		/* cipher key */	
		case 'v':
			ret = parse_cipher_iv(optarg, options);
			if (ret != 0) { 
				printf("Invalid cipher iv\n");
				return -1;
			}
			break;
		/* named pipe */	
		case 'p':
			strcpy(options->named_pipe, optarg);
			break;
		/* number of commands to process from the named pipe */	
		case 'm':
			sscanf(optarg, "%d", &options->num_commands);
			break;	
		default:
			gaf_crypto_usage(prgname);
			return -1;
		}
	}

	if (optind >= 0)
		argv[optind-1] = prgname;

	ret = optind-1;
	optind = 1; /* reset getopt lib */
	return ret;
}

static void
signal_handler(int signum)
{
	if (signum == SIGINT || signum == SIGTERM) {
		printf("\n\nSignal %d received, preparing to exit...\n",
				signum);
		signal_received = true;		
	}
}

/** Generate random key */
static void
generate_random_key(uint8_t *key, unsigned length)
{
	int fd;
	int ret;

	fd = open("/dev/urandom", O_RDONLY);
	if (fd < 0)
		rte_exit(EXIT_FAILURE, "Failed to generate random key\n");

	ret = read(fd, key, length);
	close(fd);

	if (ret != (signed)length)
		rte_exit(EXIT_FAILURE, "Failed to generate random key\n");
}

/** Generate random key; open /dev/urandom only once */
static void
generate_random_key_faster(uint8_t *key, unsigned length)
{
	static int fd = -1;
	int ret;

	if (fd == -1) {
		fd = open("/dev/urandom", O_RDONLY);
		if (fd < 0)
			rte_exit(EXIT_FAILURE, "Failed to generate random key\n");
	}

	ret = read(fd, key, length);

	if (ret != (signed)length)
		rte_exit(EXIT_FAILURE, "Failed to generate random key\n");
}

static void
reserve_key_memory(struct gaf_crypto_options *options)
{
	options->cipher_xform.cipher.key.data = options->cipher_key;

	options->cipher_iv.data = rte_malloc("cipher iv", MAX_KEY_SIZE, 0);
	if (options->cipher_iv.data == NULL)
		rte_exit(EXIT_FAILURE, "Failed to allocate memory for cipher IV");
}

static const struct rte_cryptodev_capabilities *
check_device_support_cipher_algo(const struct gaf_crypto_options *options,
		const struct rte_cryptodev_info *dev_info,
		uint8_t cdev_id)
{
	unsigned int i = 0;
	const struct rte_cryptodev_capabilities *cap = &dev_info->capabilities[0];
	enum rte_crypto_cipher_algorithm cap_cipher_algo;
	enum rte_crypto_cipher_algorithm opt_cipher_algo =
					options->cipher_xform.cipher.algo;

	while (cap->op != RTE_CRYPTO_OP_TYPE_UNDEFINED) {
		cap_cipher_algo = cap->sym.cipher.algo;
		if (cap->sym.xform_type == RTE_CRYPTO_SYM_XFORM_CIPHER) {
			if (cap_cipher_algo == opt_cipher_algo) {
				if (check_type(options, dev_info) == 0)
					break;
			}
		}
		cap = &dev_info->capabilities[++i];
	}

	if (cap->op == RTE_CRYPTO_OP_TYPE_UNDEFINED) {
		printf("Algorithm %s not supported by cryptodev %u"
			" or device not of preferred type (%s)\n",
			rte_cryptodev_get_cipher_algo_string(opt_cipher_algo),
			cdev_id,
			options->string_type);
		return NULL;
	}

	return cap;
}

static int
check_capabilities(struct gaf_crypto_options *options, uint8_t cdev_id)
{
	struct rte_cryptodev_info dev_info;
	const struct rte_cryptodev_capabilities *cap;

	rte_cryptodev_info_get(cdev_id, &dev_info);

	/* Set cipher parameters */
	if (options->xform_chain == GAF_CRYPTO_CIPHER_ONLY) {

		/* Check if device supports cipher algo. 8< */
		cap = check_device_support_cipher_algo(options, &dev_info,
						cdev_id);
		if (cap == NULL)
			return -1;

		if (check_iv_param(&cap->sym.cipher.iv_size,
				options->cipher_iv_param,
				options->cipher_iv_random_size,
				options->cipher_iv.length) != 0) {
			RTE_LOG(DEBUG, USER1,
				"Device %u does not support IV length\n",
				cdev_id);
			return -1;
		}
		/* >8 End of check if device supports cipher algo. */

		/* Check if capable cipher is supported. 8< */

		/*
		 * Check if length of provided cipher key is supported
		 * by the algorithm chosen.
		 */
		if (options->ckey_param) {
			if (check_supported_size(
					options->cipher_xform.cipher.key.length,
					cap->sym.cipher.key_size.min,
					cap->sym.cipher.key_size.max,
					cap->sym.cipher.key_size.increment)
						!= 0) {
				if (dev_info.feature_flags &
					RTE_CRYPTODEV_FF_CIPHER_WRAPPED_KEY) {
					RTE_LOG(DEBUG, USER1,
					"Key length does not match the device "
					"%u capability. Key may be wrapped\n",
					cdev_id);
				} else {
					RTE_LOG(DEBUG, USER1,
					"Key length does not match the device "
					"%u capability\n",
					cdev_id);
					return -1;
				}
			}
		}
		/* >8 End of checking if cipher is supported. */
	}

	return 0;
}

/* Check if the device is enabled by cryptodev_mask */
static int
check_cryptodev_mask(struct gaf_crypto_options *options,	
		uint8_t cdev_id)
{
	if (options->cryptodev_mask & (1 << cdev_id))
		return 0;

	return -1;
}

static int
initialize_cryptodevs(struct gaf_crypto_options *options, unsigned nb_ports,
		uint8_t *enabled_cdevs)
{
	uint8_t cdev_id, cdev_count, enabled_cdev_count = 0;
	const struct rte_cryptodev_capabilities *cap;
	unsigned int sess_sz, max_sess_sz = 0;
	uint32_t sessions_needed = 0;
	int retval;

	cdev_count = rte_cryptodev_count();
	if (cdev_count == 0) {
		printf("No crypto devices available\n");
		return -1;
	}

	for (cdev_id = 0; cdev_id < cdev_count && enabled_cdev_count < nb_ports;
			cdev_id++) {
		if (check_cryptodev_mask(options, cdev_id) < 0)
			continue;

		if (check_capabilities(options, cdev_id) < 0)
			continue;

		sess_sz = rte_cryptodev_sym_get_private_session_size(cdev_id);
		if (sess_sz > max_sess_sz)
			max_sess_sz = sess_sz;

		enabled_cdevs[cdev_id] = 1;
		enabled_cdev_count++;
	}

	for (cdev_id = 0; cdev_id < cdev_count; cdev_id++) {
		struct rte_cryptodev_qp_conf qp_conf;
		struct rte_cryptodev_info dev_info;

		if (enabled_cdevs[cdev_id] == 0)
			continue;

		if (check_cryptodev_mask(options, cdev_id) < 0)
			continue;

		if (check_capabilities(options, cdev_id) < 0)
			continue;

		retval = rte_cryptodev_socket_id(cdev_id);

		if (retval < 0) {
			printf("Invalid crypto device id used\n");
			return -1;
		}

		uint8_t socket_id = (uint8_t) retval;

		struct rte_cryptodev_config conf = {
			.nb_queue_pairs = 1,
			.socket_id = socket_id,
			.ff_disable = RTE_CRYPTODEV_FF_SECURITY,
		};

		rte_cryptodev_info_get(cdev_id, &dev_info);

		sessions_needed = enabled_cdev_count;

		if (session_pool_socket[socket_id].sess_mp == NULL) {
			char mp_name[RTE_MEMPOOL_NAMESIZE];
			snprintf(mp_name, RTE_MEMPOOL_NAMESIZE,
				"sess_mp_%u", socket_id);

			session_pool_socket[socket_id].sess_mp =
				rte_cryptodev_sym_session_pool_create(
					mp_name, sessions_needed, max_sess_sz,
					0, 0, socket_id);
			if (session_pool_socket[socket_id].sess_mp == NULL) {
				printf("Cannot create pool on socket %d\n",
					socket_id);
				return -ENOMEM;
			}

			printf("Allocated pool \"%s\" on socket %d\n",
				mp_name, socket_id);
		}

		/* Set cipher parameters */
		if (options->xform_chain == GAF_CRYPTO_CIPHER_ONLY) {
			cap = check_device_support_cipher_algo(options, &dev_info,
							cdev_id);
			options->block_size = cap->sym.cipher.block_size;

			/* Set IV if not provided from command line */
			if (options->cipher_iv_param == 0) {
				if (options->cipher_iv_random_size != -1)
					options->cipher_iv.length =
						options->cipher_iv_random_size;
				/* No size provided, use minimum size. */
				else
					options->cipher_iv.length =
						cap->sym.cipher.iv_size.min;
			}
			printf("CIPHER IV BLOCKSIZE: %u LENGTH: %u\n", options->block_size, options->cipher_iv.length);
		}

		retval = rte_cryptodev_configure(cdev_id, &conf);
		if (retval < 0) {
			printf("Failed to configure cryptodev %u", cdev_id);
			return -1;
		}

		qp_conf.nb_descriptors = 2048;
		qp_conf.mp_session = session_pool_socket[socket_id].sess_mp;

		retval = rte_cryptodev_queue_pair_setup(cdev_id, 0, &qp_conf,
				socket_id);
		if (retval < 0) {
			printf("Failed to setup queue pair %u on cryptodev %u",
					0, cdev_id);
			return -1;
		}

		retval = rte_cryptodev_start(cdev_id);
		if (retval < 0) {
			printf("Failed to start device %u: error %d\n",
					cdev_id, retval);
			return -1;
		}
	}

	return enabled_cdev_count;
}

/* Session is created and is later attached to the crypto operation. 8< */
static void *
initialize_crypto_session(struct gaf_crypto_options *options, uint8_t cdev_id)
{
	struct rte_crypto_sym_xform *first_xform;
	int retval = rte_cryptodev_socket_id(cdev_id);

	if (retval < 0)
		return NULL;

	uint8_t socket_id = (uint8_t) retval;
	
	if (options->xform_chain == GAF_CRYPTO_CIPHER_ONLY) {
		first_xform = &options->cipher_xform;
	} else {
		return NULL; // Failed
	}

	return rte_cryptodev_sym_session_create(cdev_id, first_xform,
			session_pool_socket[socket_id].sess_mp);
}
/* >8 End of creation of session. */

/* gaf_crypto_send_burst 8< */
static int
gaf_crypto_send_burst(struct lcore_queue_conf *qconf, unsigned n,
		struct gaf_crypto_params *cparams)
{
	struct rte_crypto_op **op_buffer;
	unsigned ret;

	op_buffer = (struct rte_crypto_op **)
			qconf->op_buf[cparams->dev_id].buffer;

	ret = rte_cryptodev_enqueue_burst(cparams->dev_id,
			cparams->qp_id,	op_buffer, (uint16_t) n);

	//printf("Crypto enqueued ret: %u\n", ret);

	crypto_statistics[cparams->dev_id].enqueued += ret;
	if (unlikely(ret < n)) {
		crypto_statistics[cparams->dev_id].errors += (n - ret);
		do {
			rte_pktmbuf_free(op_buffer[ret]->sym->m_src);
			rte_crypto_op_free(op_buffer[ret]);
		} while (++ret < n);
	}

	return 0;
}
/* >8 End of gaf_crypto_send_burst. */

/* Crypto enqueue. 8< */
static int
gaf_crypto_enqueue(struct rte_crypto_op *op,
		struct gaf_crypto_params *cparams)
{
	unsigned lcore_id, len;
	struct lcore_queue_conf *qconf;

	lcore_id = rte_lcore_id();
	//printf("ENQUEUE: lcore_id %u crypto dev_id %u\n", lcore_id, cparams->dev_id);

	qconf = &lcore_queue_conf[lcore_id];
	len = qconf->op_buf[cparams->dev_id].len;
	qconf->op_buf[cparams->dev_id].buffer[len] = op;
	len++;

	/* enough ops to be sent */
	// if (len == BURST_SIZE) {
	// 	gaf_crypto_send_burst(qconf, BURST_SIZE, cparams);
	// 	len = 0;
	// }

	gaf_crypto_send_burst(qconf, len, cparams);
	len = 0;

	qconf->op_buf[cparams->dev_id].len = len;
	return 0;
}
/* >8 End of crypto enqueue. */

static int
gaf_simple_crypto_enqueue(struct rte_mbuf *m,
		struct rte_crypto_op *op,
		struct gaf_crypto_params *cparams)
{
	struct rte_ether_hdr *eth_hdr;
	struct rte_ipv4_hdr *ip_hdr;

	uint32_t ipdata_offset, data_len;
	uint32_t pad_len = 0;
	char *padding;

	eth_hdr = rte_pktmbuf_mtod(m, struct rte_ether_hdr *);

	if (eth_hdr->ether_type != rte_cpu_to_be_16(RTE_ETHER_TYPE_IPV4))
		return -1;

	ipdata_offset = sizeof(struct rte_ether_hdr);

	ip_hdr = (struct rte_ipv4_hdr *)(rte_pktmbuf_mtod(m, char *) +
			ipdata_offset);

	// Assuming no IP header; everything is payload
	// ipdata_offset += (ip_hdr->version_ihl & RTE_IPV4_HDR_IHL_MASK)
	// 		* RTE_IPV4_IHL_MULTIPLIER;

	/* Zero pad data to be crypto'd so it is block aligned */
	data_len  = rte_pktmbuf_data_len(m) - ipdata_offset;
	//printf("DATA LEN: %u\n", data_len);

	if (cparams->do_cipher) {
		/*
		 * Following algorithms are block cipher algorithms,
		 * and might need padding
		 */
		switch (cparams->cipher_algo) {
		case RTE_CRYPTO_CIPHER_AES_CBC:
		case RTE_CRYPTO_CIPHER_AES_ECB:
		case RTE_CRYPTO_CIPHER_DES_CBC:
		case RTE_CRYPTO_CIPHER_3DES_CBC:
		case RTE_CRYPTO_CIPHER_3DES_ECB:
			if (data_len % cparams->block_size)
				pad_len = cparams->block_size -
					(data_len % cparams->block_size);
			break;
		case RTE_CRYPTO_CIPHER_AES_XTS:
			if (cparams->cipher_dataunit_len != 0 &&
				(data_len % cparams->cipher_dataunit_len))
				pad_len = cparams->cipher_dataunit_len -
					(data_len % cparams->cipher_dataunit_len);
			break;
		default:
			pad_len = 0;
		}

		if (pad_len) {
			padding = rte_pktmbuf_append(m, pad_len);
			if (unlikely(!padding))
				return -1;

			data_len += pad_len;
			memset(padding, 0, pad_len);
		}
	}

	/* Set crypto operation data parameters */
	rte_crypto_op_attach_sym_session(op, cparams->session);

	if (cparams->do_cipher) {
		uint8_t *iv_ptr = rte_crypto_op_ctod_offset(op, uint8_t *,
							IV_OFFSET);
		//printf("Copying Cipher IV\n");					
		/* Copy IV at the end of the crypto operation */
		// Generate random key
		//generate_random_key(cparams->cipher_iv.data, cparams->cipher_iv.length);

		rte_memcpy(iv_ptr, cparams->cipher_iv.data,
				cparams->cipher_iv.length);

		//rte_hexdump(stdout, "IV:", cparams->cipher_iv.data, cparams->cipher_iv.length);		

		/* For wireless algorithms, offset/length must be in bits */
		if (cparams->cipher_algo == RTE_CRYPTO_CIPHER_SNOW3G_UEA2 ||
				cparams->cipher_algo == RTE_CRYPTO_CIPHER_KASUMI_F8 ||
				cparams->cipher_algo == RTE_CRYPTO_CIPHER_ZUC_EEA3) {
			op->sym->cipher.data.offset = ipdata_offset << 3;
			op->sym->cipher.data.length = data_len << 3;
		} else {
			op->sym->cipher.data.offset = ipdata_offset;
			op->sym->cipher.data.length = data_len;
		}
	}

	op->sym->m_src = m;

	return gaf_crypto_enqueue(op, cparams);
}

/* Send the burst of packets on an output interface */
static int
gaf_send_burst(struct lcore_queue_conf *qconf, unsigned n,
		uint16_t port)
{
	struct rte_mbuf **pkt_buffer;
	unsigned ret;

	pkt_buffer = (struct rte_mbuf **)qconf->pkt_buf[port].buffer;

	ret = rte_eth_tx_burst(port, 0, pkt_buffer, (uint16_t)n);
	port_statistics[port].tx += ret;
	if (unlikely(ret < n)) {
		port_statistics[port].dropped += (n - ret);
		do {
			rte_pktmbuf_free(pkt_buffer[ret]);
		} while (++ret < n);
	}

	return 0;
}

/* Enqueue packets for TX and prepare them to be sent. 8< */
static int
gaf_send_packet(struct rte_mbuf *m, uint16_t port)
{
	unsigned lcore_id, len;
	struct lcore_queue_conf *qconf;

	lcore_id = rte_lcore_id();

	qconf = &lcore_queue_conf[lcore_id];
	len = qconf->pkt_buf[port].len;
	qconf->pkt_buf[port].buffer[len] = m;
	len++;

	/* enough pkts to be sent */
	if (unlikely(len == BURST_SIZE)) {
		gaf_send_burst(qconf, BURST_SIZE, port);
		len = 0;
	}

	qconf->pkt_buf[port].len = len;
	return 0;
}
/* >8 End of Enqueuing packets for TX. */


static void
gaf_simple_forward(struct rte_mbuf *m, uint16_t portid,
		struct gaf_crypto_options *options)
{
	uint16_t dst_port;
	uint32_t pad_len;
	struct rte_ipv4_hdr *ip_hdr;
	uint32_t ipdata_offset = sizeof(struct rte_ether_hdr);

	ip_hdr = (struct rte_ipv4_hdr *)(rte_pktmbuf_mtod(m, char *) +
					 ipdata_offset);
	dst_port = portid ^ 1; // 0 --> 1; 1 --> 0

	if (options->cipher_xform.cipher.op == RTE_CRYPTO_CIPHER_OP_DECRYPT) {
		pad_len = m->pkt_len - rte_be_to_cpu_16(ip_hdr->total_length) -
			  ipdata_offset;
		rte_pktmbuf_trim(m, pad_len);
	}

	gaf_send_packet(m, dst_port);
}

/* Launch a function on lcore. 8< */
static int
lcore_send(__rte_unused void *arg)
{
	unsigned lcore_id;
	lcore_id = rte_lcore_id();
	struct gaf_crypto_options *options = (struct gaf_crypto_options *) arg;

	/* Create a mempool */
	struct rte_mempool *mbuf_pool;
	mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL2", NUM_MBUFS,
		MBUF_CACHE_SIZE, 0, RTE_MBUF_DEFAULT_BUF_SIZE,
		rte_socket_id());

	if (mbuf_pool == NULL)
		rte_exit(EXIT_FAILURE, "Cannot init mbuf pool\n");

	uint16_t portIDs[MAX_PORTS] = {0, 1};	
	struct rte_ether_addr s_addr[MAX_PORTS];

	// Get the MAC addresses of the ports
	int i, retval;
	for (i = 0; i < MAX_PORTS; i++) {
		retval = rte_eth_macaddr_get(portIDs[i], &s_addr[portIDs[i]]);
		if (retval != 0)
			return retval;
	}

	// Identify the portID to transmit packets based on the sender MAC address
	uint16_t sender_portID;
	for (i = 0; i < MAX_PORTS; i++) {
		if (rte_is_same_ether_addr(&options->src_eth_addr, &s_addr[portIDs[i]]))
			sender_portID = portIDs[i];
	}

	// Open the input file to transmit
	FILE *fp = fopen(options->input_file, "r");
	assert(fp);

	char filebuf[BURST_SIZE][MAX_LINE_LEN];
	int line = 0;
	while (!feof(fp)) {
		if (fgets(filebuf[line], MAX_LINE_LEN, fp) != NULL) {
			//printf("%s\n", filebuf[line]);
	
			if (line == BURST_SIZE - 1) {

				//struct rte_ether_addr s_addr = {0xb8,0xce,0xf6,0x5d,0x45,0x6e};
				// printf("Size ether_addr: %lu\n", sizeof(s_addr[1]));
				
				uint16_t ether_type = 0x86DD; // IPV6
				struct rte_ether_hdr *eth_hdr;
				
				/* Reference: https://zenhox.github.io/2018/01/25/dpdk-pktSR/ */
				struct Message {
					char data[MAX_LINE_LEN];
				};
				
				struct Message *msg;
				struct rte_mbuf *pkt[BURST_SIZE];

				/* Create a burst of packets */
				int i;
				for(i = 0; i < BURST_SIZE; i++) {
					pkt[i] = rte_pktmbuf_alloc(mbuf_pool);
					eth_hdr = rte_pktmbuf_mtod(pkt[i], struct rte_ether_hdr*);
					eth_hdr->dst_addr = options->dst_eth_addr;
					eth_hdr->src_addr = options->src_eth_addr;
					eth_hdr->ether_type = ether_type;
					msg = (struct Message*) (rte_pktmbuf_mtod(pkt[i], char*) + sizeof(struct rte_ether_hdr));
					//*msg = obj;
					memcpy(msg, filebuf[i], MAX_LINE_LEN);
					int pkt_size = sizeof(struct Message) + sizeof(struct rte_ether_hdr);
					pkt[i]->data_len = pkt_size;
					pkt[i]->pkt_len = pkt_size;
				}

				uint16_t nb_tx = rte_eth_tx_burst(sender_portID, 0, pkt, BURST_SIZE);
				printf("PortID: %u, number of transmitted packets: %u\n", sender_portID, nb_tx);

				for(i = 0; i < BURST_SIZE; i++)
					rte_pktmbuf_free(pkt[i]);
				
				line = 0;
			}
			else {
				line++;
			}
		}
	}
	fclose(fp);

	printf("Reading file on core %u\n", lcore_id);
	return 0;
}


/* reads file data (blocking), encrypts/decrypts, and stores the output in a file */
static int
gaf_crypto_process_file_with_pipes(__rte_unused void *arg)
{
	unsigned lcore_id = rte_lcore_id();
	struct gaf_crypto_options *options = (struct gaf_crypto_options *) arg;

	struct rte_mbuf *m, *pkts_burst[BURST_SIZE];
	struct rte_crypto_op *ops_burst[BURST_SIZE];

	unsigned int i, j, nb_rx;
	uint16_t portid;
	struct lcore_queue_conf *qconf = &lcore_queue_conf[lcore_id];
	struct gaf_crypto_params *cparams;
	struct gaf_crypto_params port_cparams[qconf->nb_crypto_devs];
	void *session;

	if (qconf->nb_rx_ports == 0) {
		RTE_LOG(INFO, CRYPTODEV, "lcore %u has nothing to do\n", lcore_id);
		return -1;
	}

	RTE_LOG(INFO, CRYPTODEV, "entering main loop on lcore %u\n", lcore_id);

	for (i = 0; i < qconf->nb_rx_ports; i++) {
		printf("Lcore: %u, RX ports: %u, cryptodevs: %u \n", lcore_id, qconf->nb_rx_ports, qconf->nb_crypto_devs);
		portid = qconf->rx_port_list[i];
		RTE_LOG(INFO, CRYPTODEV, " -- lcoreid=%u portid=%u\n", lcore_id,
			portid);
	}

	// Code assumes crypto devs are mapped to cores 0, 1, 2, ...
	for (i = 0; i < qconf->nb_crypto_devs; i++) {
		printf("Crypto dev id %d\n", i);
		port_cparams[i].do_cipher = 0;

		switch (options->xform_chain) {
		case GAF_CRYPTO_CIPHER_ONLY:
			port_cparams[i].do_cipher = 1;
			break;
		default:
			printf("Unsupport chain operation\n");
			return -1;	
		}

		port_cparams[i].dev_id = qconf->cryptodev_list[i];
		port_cparams[i].qp_id = 0;

		port_cparams[i].block_size = options->block_size;

		if (port_cparams[i].do_cipher) {
			port_cparams[i].cipher_iv.data = options->cipher_iv.data;
			port_cparams[i].cipher_iv.length = options->cipher_iv.length;
			printf("PORT CIPHER IV: %u\n", options->cipher_iv.length);

			if (!options->cipher_iv_param)
				generate_random_key(port_cparams[i].cipher_iv.data,
						port_cparams[i].cipher_iv.length);

			port_cparams[i].cipher_algo = options->cipher_xform.cipher.algo;
			port_cparams[i].cipher_dataunit_len =
				options->cipher_xform.cipher.dataunit_len;
			/* Set IV parameters */
			options->cipher_xform.cipher.iv.offset = IV_OFFSET;
			options->cipher_xform.cipher.iv.length =
						options->cipher_iv.length;
		}

		session = initialize_crypto_session(options,
				port_cparams[i].dev_id);
		if (session == NULL)
			rte_exit(EXIT_FAILURE, "Failed to initialize crypto session\n");

		port_cparams[i].session = session;

		RTE_LOG(INFO, CRYPTODEV, " -- lcoreid=%u cryptoid=%u\n", lcore_id,
				port_cparams[i].dev_id);
	}

	printf("Finished initialization of crypto sessions\n");

	long start, end;
	struct timeval timecheck;

	bool is_streaming = false;
	char named_pipe[MAX_STR_LEN+sizeof(unsigned int)];
	// Input/output/crypto op will be provided in the pipe
	if (strcmp(options->named_pipe, "")) {
		sprintf(named_pipe, "%s_%u", options->named_pipe, lcore_id);
		unlink(named_pipe);
		umask(0);
		mkfifo(named_pipe, 0777);
		printf("Created named pipe: %s\n", named_pipe);
		is_streaming = true;
	}

	do {
		FILE *fpin, *fpout;
		char read_buffer[MAX_MSG_LEN];
		char input_file[MAX_STR_LEN], output_file[MAX_STR_LEN];
		enum rte_crypto_cipher_operation cipher_op;

		if (is_streaming == true) {
			char crypto_operation[MAX_STR_LEN];
			FILE *fp = fopen(named_pipe, "r");
			int m = fscanf(fp, "%s %s %s", input_file, output_file, crypto_operation);
			if (m < 3) {
				printf("Incorrect command format: %s %s %s\n", input_file, output_file, crypto_operation);
				fclose(fp);
				return 0;
			}
			fclose(fp);
			parse_cipher_op(crypto_operation, &cipher_op);
			printf("INPUT: %s, OUTPUT: %s, OP: %d\n", input_file, output_file, cipher_op);
		}
		else {
			// Command line args are copied
			strcpy(input_file, options->input_file);
			strcpy(output_file, options->output_file);
			cipher_op = options->cipher_xform.cipher.op;
		}

		gettimeofday(&timecheck, NULL);
		start = (long)timecheck.tv_sec * 1000 + (long)timecheck.tv_usec / 1000;

		// Open the input file: required
		fpin = fopen(input_file, "rb");
		assert(fpin);
		printf("Opened file %s\n", input_file);

		// Create the output named pipe if provided
		if (strstr(output_file, "/tmp/") != NULL) {
			// Output file is a named pipe
			unlink(output_file);
			mkfifo(output_file, 0777);
		}

		// Open the output file: required
		fpout = fopen(output_file, "wb");
		assert(fpout);
		printf("Opened file %s\n", output_file);

		// Based on lcore_id; otherwise causes error
		portid = qconf->rx_port_list[0];
		cparams = &port_cparams[0];

		// Separate memory for each worker to prevent overwriting each other's IV
		cparams->cipher_iv.data = rte_malloc("cipher iv", MAX_KEY_SIZE, 0);
		if (cparams->cipher_iv.data == NULL)
			rte_exit(EXIT_FAILURE, "Failed to allocate memory for cipher IV");

		size_t n;

		while (!feof(fpin)) {
			if (cipher_op == RTE_CRYPTO_CIPHER_OP_ENCRYPT) {
				n = fread(read_buffer, 1, MAX_MSG_LEN, fpin);
				//printf("Encrypt -- READ a line: %ld %d\n", n, k);
				if (n == 0) continue;

				// Generate random IV
				generate_random_key(cparams->cipher_iv.data, cparams->cipher_iv.length);
				//rte_hexdump(stdout, "IV:", cparams->cipher_iv.data, cparams->cipher_iv.length);		
			}
			else if (cipher_op == RTE_CRYPTO_CIPHER_OP_DECRYPT) {
				size_t cipher_size;
				n = fread(&cipher_size, 1, HEADER_SIZE, fpin);
				//printf("Decrypt -- READ a line: %ld %d\n", n, k);
				if (n == 0) continue;

				// Read IV from the cipher stream
				n = fread(cparams->cipher_iv.data, 1, cparams->cipher_iv.length, fpin);
				if (n == 0) continue;
				//printf("CIPHER size: %ld Header size: %ld\n", cipher_size);
				
				n = fread(read_buffer, 1, cipher_size, fpin);
				//printf("Decrypt -- READ a line: %ld %d\n", n, k);
				if (n == 0) continue;
			}
			else {
				printf("Unsupported operation\n");
				return -1;
			}

			if (n > 0) {
				/*
				* Read packet from RX queues
				*/

				nb_rx = 1;

				for (j = 0; j < nb_rx; j++) {
					uint16_t ether_type = 0x0008; // IPV4 - small endian
					pkts_burst[j] = rte_pktmbuf_alloc(gaf_crypto_mbuf_pool);
					assert(pkts_burst[j]);

					struct rte_ether_hdr *eth_hdr = rte_pktmbuf_mtod(pkts_burst[j], struct rte_ether_hdr*);
					eth_hdr->dst_addr = options->dst_eth_addr;
					eth_hdr->src_addr = options->src_eth_addr;
					eth_hdr->ether_type = ether_type;
					char *msg = (char *) (rte_pktmbuf_mtod(pkts_burst[j], char*) + sizeof(struct rte_ether_hdr));
					memcpy(msg, read_buffer, n);
					int pkt_size = n + sizeof(struct rte_ether_hdr);
					pkts_burst[j]->data_len = pkt_size;
					pkts_burst[j]->pkt_len = pkt_size;
				}
				port_statistics[portid].rx += nb_rx;

				/* Allocate and fillcrypto operations. 8< */
				if (nb_rx) {
					/*
					* If we can't allocate a crypto_ops, then drop
					* the rest of the burst and dequeue and
					* process the packets to free offload structs
					*/
					if (rte_crypto_op_bulk_alloc(
							gaf_crypto_op_pool,
							RTE_CRYPTO_OP_TYPE_SYMMETRIC,
							ops_burst, nb_rx) !=
									nb_rx) {
						for (j = 0; j < nb_rx; j++)
							rte_pktmbuf_free(pkts_burst[j]);

						nb_rx = 0;
					}
					/* >8 End of crypto operation allocated and filled. */

					/* Enqueue packets from Crypto device*/
					for (j = 0; j < nb_rx; j++) {
						m = pkts_burst[j];

						gaf_simple_crypto_enqueue(m,
								ops_burst[j], cparams);

					}
					// if (nb_rx)
					// 	printf("Finished crypto enqueue %u\n", nb_rx);
				}

				do {
					/* Dequeue packets from Crypto device. 8< */
					nb_rx = rte_cryptodev_dequeue_burst(
							cparams->dev_id, cparams->qp_id,
							ops_burst, BURST_SIZE);
					// if (nb_rx) 
					// 	printf("Finished crypto dequeue... %u\n", nb_rx);

					crypto_statistics[cparams->dev_id].dequeued +=
							nb_rx;

					for (j = 0; j < nb_rx; j++) {
						m = ops_burst[j]->sym->m_src;
						struct rte_ether_hdr *eth_hdr = rte_pktmbuf_mtod(m, struct rte_ether_hdr*);

						if (cipher_op == RTE_CRYPTO_CIPHER_OP_ENCRYPT) {
							// Write the number of bytes followed by the cipher text
							char *msg = (char *) (rte_pktmbuf_mtod(m, char*) + sizeof(struct rte_ether_hdr));
							size_t cipher_len = m->data_len - sizeof(struct rte_ether_hdr);
							// Write the header
							size_t num_bytes_header = fwrite((void *) &cipher_len, 1, sizeof(cipher_len), fpout);
							// Write the IV
							size_t num_bytes_IV = fwrite((void *) cparams->cipher_iv.data, 1, cparams->cipher_iv.length, fpout);
							// Write the encrypted payload
							size_t num_bytes_payload = fwrite(msg, 1, cipher_len, fpout);
							// printf("MBUF header: %lu, cipher_len: %lu, cipher_len_written: %lu, data_len: %u, packet_len: %u, IV_len: %u\n", 
							// 	num_bytes_header, cipher_len, num_bytes_payload, m->data_len, m->pkt_len, num_bytes_IV);	
						}
						else if (cipher_op == RTE_CRYPTO_CIPHER_OP_DECRYPT) {
							// Write the plain text
							char *msg = (char *) (rte_pktmbuf_mtod(m, char*) + sizeof(struct rte_ether_hdr));
							size_t plain_text_len = m->data_len - sizeof(struct rte_ether_hdr);
							size_t num_bytes_payload = fwrite(msg, 1, plain_text_len, fpout);
							// printf("MBUF plain_text_len: %lu, text_written: %lu, data_len: %u, packet_len: %u\n", 
							// 	plain_text_len, num_bytes_payload, m->data_len, m->pkt_len);
						}
						else {
							printf("Unsupported operation\n");
							return -1;
						}
						rte_pktmbuf_free(m); // Free allocated memory from mbuf_pool; otherwise will run out of memory
						rte_crypto_op_free(ops_burst[j]);
					}
				} while (nb_rx == BURST_SIZE);
				/* >8 End of dequeue packets from crypto device. */
			}
		}		

		gettimeofday(&timecheck, NULL);
		end = (long)timecheck.tv_sec * 1000 + (long)timecheck.tv_usec / 1000;
		printf("Time taken: %ld msec\n", (end - start));

		printf("Port statistics: enqueued: %lu dequeued: %lu\n",
			port_statistics[portid].rx, crypto_statistics[cparams->dev_id].dequeued);
		port_statistics[portid].rx = 0;
		crypto_statistics[cparams->dev_id].dequeued = 0;
		
		fclose(fpout);
		fclose(fpin);

		gettimeofday(&timecheck, NULL);
		long end2 = (long)timecheck.tv_sec * 1000 + (long)timecheck.tv_usec / 1000;
		printf("File close time taken: %ld msec\n", (end2 - end));
		
		if (unlikely(signal_received))
			break;	
	} while (is_streaming == true);

	return 0;
}

/*
 * The main function, which does initialization and calls the per-lcore
 * functions.
 */
int
main(int argc, char *argv[])
{
	struct rte_mempool *mbuf_pool;
	unsigned nb_ports;
	uint8_t nb_cryptodevs, cdev_id;
	uint16_t portid;
	unsigned lcore_id, rx_lcore_id = 0, first_lcore_id;
	struct lcore_queue_conf *qconf = NULL;

	int ret, enabled_cdevcount, enabled_portcount;
	uint8_t enabled_cdevs[RTE_CRYPTO_MAX_DEVS] = {0};

	/* Initializion the Environment Abstraction Layer (EAL). 8< */
	ret = rte_eal_init(argc, argv);
	if (ret < 0)
		rte_exit(EXIT_FAILURE, "Error with EAL initialization\n");
	/* >8 End of initialization the Environment Abstraction Layer (EAL). */

	argc -= ret;
	argv += ret;

	signal(SIGINT, signal_handler);
	signal(SIGTERM, signal_handler);

	/* parse application arguments (after the EAL ones) */
	struct gaf_crypto_options options = {.xform_chain = GAF_CRYPTO_CIPHER_ONLY, 
								  .cipher_xform.type = RTE_CRYPTO_SYM_XFORM_CIPHER,
								  .cipher_xform.next = NULL,
								  .cipher_xform.cipher.dataunit_len = 0,
								  .cipher_iv_param = 0, 
								  .cryptodev_mask = UINT64_MAX, 
								  .cipher_iv_random_size = -1,
								  .type = CDEV_TYPE_SW,
								  .string_type = "SW",
								  .nb_ports_per_lcore = 1,
								  .input_file = "",
								  .output_file = "",
								  .named_pipe = "",
								  .num_commands = 1}; 

	/* reserve memory for Cipher/Auth key and IV */
	reserve_key_memory(&options);
	
	ret = gaf_crypto_parse_args(argc, argv, &options);
	if (ret < 0)
		rte_exit(EXIT_FAILURE, "Invalid GAF_CRYPTO arguments\n");
	/* >8 End of init EAL. */

	/* create crypto op pool */
	gaf_crypto_op_pool = rte_crypto_op_pool_create("crypto_op_pool",
			RTE_CRYPTO_OP_TYPE_SYMMETRIC, NUM_MBUFS, 128, MAXIMUM_IV_LENGTH,
			rte_socket_id());
	if (gaf_crypto_op_pool == NULL)
		rte_exit(EXIT_FAILURE, "Cannot create crypto op pool\n");

	/* Check that there is an even number of ports to send/receive on. */
	nb_ports = rte_eth_dev_count_avail();
	if (nb_ports < 2 || (nb_ports & 1))
		rte_exit(EXIT_FAILURE, "Error: number of ports must be even\n");

	/* Creates a new mempool in memory to hold the mbufs. */

	/* Allocates mempool to hold the mbufs. 8< */
	gaf_crypto_mbuf_pool = rte_pktmbuf_pool_create("MBUF_POOL", NUM_MBUFS * nb_ports,
		MBUF_CACHE_SIZE, 0, RTE_MBUF_DEFAULT_BUF_SIZE, rte_socket_id());
	/* >8 End of allocating mempool to hold mbuf. */

	if (gaf_crypto_mbuf_pool == NULL)
		rte_exit(EXIT_FAILURE, "Cannot create mbuf pool\n");

	/* Initializing all ports. 8< */
	enabled_portcount = 0;
	RTE_ETH_FOREACH_DEV(portid) {
		if (port_init(portid, gaf_crypto_mbuf_pool) != 0)
			rte_exit(EXIT_FAILURE, "Cannot init port %"PRIu16 "\n", portid);
		enabled_portcount++;
	}				
	/* >8 End of initializing all ports. */

	if (rte_lcore_count() > 1)
		printf("\nWARNING: Too many lcores enabled. Only 1 used.\n");

	/* Initialize the port/queue configuration of each logical core */
	RTE_ETH_FOREACH_DEV(portid) {
		/* get the lcore_id for this port */
		while (rte_lcore_is_enabled(rx_lcore_id) == 0 ||
				lcore_queue_conf[rx_lcore_id].nb_rx_ports ==
				options.nb_ports_per_lcore) {
			rx_lcore_id++;
			if (rx_lcore_id >= RTE_MAX_LCORE)
				rte_exit(EXIT_FAILURE,
						"Not enough cores\n");
		}

		/* Assigned a new logical core in the loop above. */
		if (qconf != &lcore_queue_conf[rx_lcore_id])
			qconf = &lcore_queue_conf[rx_lcore_id];

		qconf->rx_port_list[qconf->nb_rx_ports] = portid;
		qconf->nb_rx_ports++;

		printf("Lcore %u: RX port %u\n", rx_lcore_id, portid);
	}

	/* Enable Crypto devices */
	enabled_cdevcount = initialize_cryptodevs(&options, enabled_portcount,
		enabled_cdevs);
	if (enabled_cdevcount < 0)
		rte_exit(EXIT_FAILURE, "Failed to initialize crypto devices\n");

	printf("Number of enabled crypto devices: %d\n", enabled_cdevcount);

	printf("CIPHER IV OPTIONS: %u\n", options.cipher_iv.length);

	// if (enabled_cdevcount < enabled_portcount)
	// 	rte_exit(EXIT_FAILURE, "Number of capable crypto devices (%d) "
	// 			"has to be more or equal to number of ports (%d)\n",
	// 			enabled_cdevcount, enabled_portcount);

	nb_cryptodevs = rte_cryptodev_count();
	printf("Num crypto devices: %u\n", nb_cryptodevs);

	// Select the first lcore_id
	RTE_LCORE_FOREACH(lcore_id) {
		first_lcore_id = lcore_id;
		break;
	}

	/* Initialize the port/cryptodev configuration of each logical core */

	for (rx_lcore_id = first_lcore_id, qconf = NULL, cdev_id = 0;
			cdev_id < nb_cryptodevs && enabled_cdevcount;
			cdev_id++) {
		/* Crypto op not supported by crypto device */
		if (!enabled_cdevs[cdev_id]) {
			printf("Cryptodevice disabled: %u\n", cdev_id);
			continue;
		}

		/* get the lcore_id for this port */
		while (rte_lcore_is_enabled(rx_lcore_id) == 0 ||
				lcore_queue_conf[rx_lcore_id].nb_crypto_devs ==
				options.nb_ports_per_lcore) {
			printf("LCORE: %u, NUM CRYPTO DEVS: %u\n", rx_lcore_id, lcore_queue_conf[rx_lcore_id].nb_crypto_devs);
			rx_lcore_id++;
			if (rx_lcore_id >= RTE_MAX_LCORE)
				rte_exit(EXIT_FAILURE,
						"Not enough cores\n");
		}

		/* Assigned a new logical core in the loop above. */
		if (qconf != &lcore_queue_conf[rx_lcore_id])
			qconf = &lcore_queue_conf[rx_lcore_id];

		qconf->cryptodev_list[qconf->nb_crypto_devs] = cdev_id;
		qconf->nb_crypto_devs++;

		enabled_cdevcount--;

		printf("Lcore %u: cryptodev %u\n", rx_lcore_id,
				(unsigned) cdev_id);
	}

	display_cipher_info(&options);

	// If named pipe is specified
	if (strcmp(options.named_pipe, "")) {
		rte_eal_mp_remote_launch(gaf_crypto_process_file_with_pipes, (void *)&options,
				CALL_MAIN);
		RTE_LCORE_FOREACH_WORKER(lcore_id) {
			if (rte_eal_wait_lcore(lcore_id) < 0)
				return -1;
		}
	}
	else {
		unsigned int worker_core_id = first_lcore_id + 1;	
		rte_eal_remote_launch(gaf_crypto_process_file_with_pipes, &options, worker_core_id);

		if (rte_eal_wait_lcore(worker_core_id) < 0)
			printf("SOME ERROR\n");
	}

	/* clean up the EAL */
	rte_eal_cleanup();

	return 0;
}
