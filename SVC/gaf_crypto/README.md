
# To rebuild the examples/ folder

```
$ cd build
$ sudo meson compile
```

OR

```
$ meson -Dexamples=all build
$ cd build
$ sudo ninja install
$ sudo ldconfig
```

# Create interleaved FASTQ file 
1. To create an interleaved FASTQ file from paired-end sequences
```
wget https://github.com/telatin/seqfu2/releases/download/v1.16.0/SeqFu-v1.16.0-Linux-x86_64.zip
unzip SeqFu-v1.16.0-Linux-x86_64.zip -d SeqFu
SeqFu/seqfu  interleave -1 ERR062934_1.fastq.gz -2 ERR062934_2.fastq.gz | gzip > ERR062934_interleaved.fastq.gz
```

# Steps to execute SVC-Server

1. Start gaf-crypto
```
sudo ${HOME}/dpdk/examples/dpdk-gaf_crypto -l 0-2 -n 4 --vdev "crypto_aesni_mb0"  --vdev "crypto_aesni_mb1" -- -a aes-ctr -k 00:01:02:03:04:05:06:07:08:09:0a:0b:0c:0d:0e:0f -v 00:01:02:00:00:00:00:00:00:00:00:00:00:00:00:02 -p /tmp/gaf_crypto
```

2. To encrypt the interleaved FASTQ file (or see instructions below with SVC-Client)
```
echo '/home/ubuntu/ERR062934_interleaved.fastq.gz /home/ubuntu/ERR062934_interleaved.fastq.gz.en ENCRYPT' > /tmp/gaf_crypto_1
```

3. To run variant calling (using Freebayes) on encrypted input (ciphertext); produces encrypted VCF
```
${HOME}/dpdk/examples/gaf_crypto/pipeline/run_variant_analysis.sh /home/ubuntu/ERR062934_interleaved.fastq.gz.en F 1 >& /tmp/cipher.log
```

4. To run variant calling (using Freebayes) on plaintext input; produces plaintext VCF
```
${HOME}/dpdk/examples/gaf_crypto/pipeline/run_variant_analysis.sh /home/ubuntu/ERR062934_interleaved.fastq.gz F 0 >& /tmp/plain.log
```

# Steps to execute SVC-Client 

## Installation

1. Tested OS: Ubuntu 22.04
2. Install PyCryptodome for `scrypt`
```
$ pip3 install PyCryptodome
```
3. Build the client code (CPU-based encryption/decryption)
```
$ cd ${HOME}/dpdk/examples/gaf_crypto/client/; make
```

## Usage
1. First step is to generate a key (16 or 32 bytes)
```
$ python3 ${HOME}/dpdk/examples/gaf_crypto/client/generate_key.py 16
Password: 
>>>>>>>>>>>>>>
ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
<<<<<<<<<<<<<<
👉 Done!
```
2. Use the above generated key to encrypt the file `ERR062934_interleaved.fastq.gz`
```
$ ${HOME}/dpdk/examples/gaf_crypto/client/client ERR062934_interleaved.fastq.gz ERR062934_interleaved.fastq.gz.en en ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
```
3. Run Steps 1, 3 and 4 shown above (for `SVC-Server`) using the generated key

4. Do the following to decrypt the output VCF file `output.vcf.en`
```
$ ${HOME}/dpdk/examples/gaf_crypto/client/client output.vcf.en output.vcf.de de ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
```



# References

1. https://pycryptodome.readthedocs.io
2. https://github.com/kokke/tiny-AES-c

