SUBDIRS := gen_cnode

.PHONY: subdirs $(SUBDIRS)

all clean install: $(SUBDIRS) ;

clobber:
	rm -rf gen_cnode

gen_cnode_git:
	if test -d gen_cnode; then cd ./gen_cnode && git pull && cd ..; else git clone git://github.com/called-shot/gen_cnode.git ./gen_cnode; fi

gen_cnode: gen_cnode_git
$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
