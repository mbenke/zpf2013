SUBDIRS:=`seq -f "%02.f" -s " " 1 5` 
#SHELL:=/bin/bash

all: subdirs

subdirs:
	for i in $(SUBDIRS); do \
	  make -C $$i all;\
	done

echo:
	-echo "SHELL: " $(SHELL)
	-echo "MAKE: " $(MAKE)
