BINARIES=Node
include ../standard.mk

run: $(BINARIES)
	sudo xl create Node.config
	sleep 2
	sudo xl dmesg -c
	-sudo xl destroy Node
