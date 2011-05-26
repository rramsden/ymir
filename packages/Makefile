SUBDIRS := gen_cnode

.PHONY: subdirs $(SUBDIRS)

all clean install: $(SUBDIRS) ;

clobber:
	rm -rf $(SUBDIRS)

gen_cnode_git:
	test -d gen_cnode || git clone ssh://gitosis@acalledshot.org/gen_cnode.git ./gen_cnode

gen_cnode: gen_cnode_git
$(SUBDIRS):
	$(warning $(MAKECMDGOALS))
	$(MAKE) -C $@ $(MAKECMDGOALS)
