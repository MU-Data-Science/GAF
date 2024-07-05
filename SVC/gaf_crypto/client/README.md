
# Installation

1. Tested OS: Ubuntu 22.04
2. Install PyCryptodome for `scrypt`
```
$ pip3 install PyCryptodome
```
3. Build the code
```
$ cd ${HOME}/dpdk/examples/gaf_crypto/client/; make
```

# Usage
1. First step is to generate a key (16 or 32 bytes)
```
$ python3 ${HOME}/dpdk/examples/gaf_crypto/client/generate_key.py 16
Password: 
>>>>>>>>>>>>>>
ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
<<<<<<<<<<<<<<
👉 Done!
```
2. Use the above generated key to encrypt the file `ERR062934_1.fastq.gz`
```
$ ${HOME}/dpdk/examples/gaf_crypto/client/client ERR062934_1.fastq.gz ERR062934_1.fastq.gz.en en ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
```
3. Do the following to decrypt the file `ERR062934_1.fastq.gz.en`
```
$ ${HOME}/dpdk/examples/gaf_crypto/client/client ERR062934_1.fastq.gz.en ERR062934_1.fastq.gz.de de ef:9d:00:08:7c:60:82:7a:75:46:44:4b:04:b2:08:6f
```
4. Compare the original file and the decrypted file
```
$ cmp ERR062934_1.fastq.gz ERR062934_1.fastq.gz.de 
```
# References

1. https://pycryptodome.readthedocs.io
2. https://github.com/kokke/tiny-AES-c
