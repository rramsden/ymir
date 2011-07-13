include ./Makedefs

SUBDIRS := packages src lib

.PHONY: subdirs $(SUBDIRS)

all clean clobber: $(SUBDIRS) ;

makedir:
	install --mode=755 -d $(INSTALL_DIR)
	install --mode=755 -d $(LIB_DIR)
	install --mode=755 -d $(EBIN_DIR)

install: makedir $(SUBDIRS)
	install -m 644 -D plugins.cfg $(INSTALL_DIR) 

$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
