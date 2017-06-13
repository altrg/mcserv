mcserv
=====

An Erlang multicast file transfer application

Build
-----
    $ make

Start server
-----
    $ make run

Server config
-----
```
config/sys.config:
{mcserv,
  [
   {rate, 1000}, % stream rate, KB/s
   {packet_size, 1470}, % UDP packet size
   {file_path, "/tmp/mcserv.bin"}, % file to stream
   {mcast_address, {224, 2, 2, 4}}, % multicast address
   {mcast_port, 1111}, % multicast port
   {http_port, 8224} % http port to listen to metadata requests
  ]}
```
Client usage
---
```
$ _build/client/bin/mcclient
Usage: mcclient [OPTION] URL

  -p  path to save a file, default current directory
  -t  timeout to detect lost packets, ms, default 5000
  -l  packet loss emulation, percentage, can be float, default 0
```
Example
---
This example receives a file using metadata from the multicast server 192.168.2.1
   
    $ _build/client/bin/mcclient -l 0.1 -t 1000 http://192.168.2.1:8224

Client emulates 0.1% packets loss and uses 1s correction timeout.
