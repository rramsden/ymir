include ./Makedefs

SUBDIRS := packages src lib

.PHONY: subdirs $(SUBDIRS)

all clean install clobber: $(SUBDIRS) ;

$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
