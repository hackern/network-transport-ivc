BINARIES=Peer
include ../standard.mk

run: $(BINARIES)
	sudo xl create Peer.config
	sleep 2
	sudo xl dmesg -c
	-sudo xl destroy Peer
