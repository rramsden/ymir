include ../Makedefs

APP_FILES := $(wildcard *.app)
ERL_FILES := $(wildcard *.erl)
BEAM_TARGETS := $(addsuffix .beam, $(basename $(ERL_FILES)))
APP_TARGETS := $(wildcard *.app)

default: all

all: $(BEAM_TARGETS) ;

%.beam: %.erl
	erlc -W $<

clean: 
	rm -f $(BEAM_TARGETS)
	rm -f ./*.*~

clobber: clean ;

install: all ;
#	install -m 0755 $(ERL_FILES) $(SRC_DIR)
#	install -m 0755 $(APP_FILES) $(EBIN_DIR)
#	install -m 0755 $(BEAM_TARGETS) $(EBIN_DIR)

uninstall: ;
