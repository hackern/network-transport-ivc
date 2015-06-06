BINARIES=Peer1
include standard.mk

run: $(BINARIES)
	sudo xl create Peer1.config
	sleep 5
	sudo xl dmesg -c
