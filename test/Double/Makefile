BINARIES=Peer1 Peer2
include ../standard.mk

run: $(BINARIES)
	sudo xl create Peer1.config
	sudo xl create Peer2.config
	sleep 2
	sudo xl dmesg -c
	-sudo xl destroy Peer1
	-sudo xl destroy Peer2
