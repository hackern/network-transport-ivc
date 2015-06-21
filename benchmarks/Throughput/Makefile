BINARIES=Main
include ../standard.mk

run: $(BINARIES)
	~/xenUtil/xenutil
	-sudo xenstore-rm /process/counter-pid
	sudo xl create Node1.config
	sudo xl create Node2.config
	sleep 2
	sudo xl dmesg -c
	-sudo xl destroy Node1
	-sudo xl destroy Node2
