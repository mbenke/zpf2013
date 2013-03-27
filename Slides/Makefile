SUBDIRS := ${sort ${dir ${wildcard ./*/Makefile}}}

.PHONY: all subdirs $(SUBDIRS)


all: subdirs


subdirs: $(SUBDIRS)


$(SUBDIRS):
	$(MAKE) -C $@

