SUBDIRS:= 1 2
#SHELL:=/bin/bash

all: subdirs

subdirs:
	for i in $(SUBDIRS); do \
	  make -C $$i all;\
	done

echo:
	-echo "SHELL: " $(SHELL)
	-echo "MAKE: " $(MAKE)