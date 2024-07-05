#!/usr/bin/env python3
import sys
from Crypto.Protocol.KDF import scrypt
from Crypto.Random import get_random_bytes
from getpass import getpass

def main():
    if (len(sys.argv) < 2):
        usage(sys.argv[0])
        sys.exit(2)
    
    keylen = int(sys.argv[1])
    password = getpass().encode('ascii')

    salt = get_random_bytes(16)
    key = scrypt(password, salt, keylen, N=2**14, r=8, p=1)
    key = key.hex()
    print(">>>>>>>>>>>>>>")
    print(':'.join(key[i:i+2] for i in range(0, len(key), 2)))
    print("<<<<<<<<<<<<<<")

def usage(prog_name):
    print("python3 {} <key_length (in bytes)>".format(prog_name))
    print("")

if __name__ == "__main__":
    main()
    print("👉 Done!")