#!/bin/bash

#Setup NAT64 based on https://nat64.net/
sudo sed -i '/nameserver/d' /etc/resolv.conf
sudo sh -c 'echo nameserver 2a00:1098:2c::1 >> /etc/resolv.conf' 
sudo sh -c 'echo nameserver 2a01:4f8:c2c:123f::1 >> /etc/resolv.conf' 
sudo sh -c 'echo nameserver 2a00:1098:2b::1 >> /etc/resolv.conf'
