include ../Makedefs

LIB_TARGETS := libOgreManager.so 

CFLAGS += -I../include
CFLAGS += -I../packages/gen_cnode/include
CFLAGS += -I$(ERL_INTERFACE_INCLUDE)

ifeq ($(strip $(KERNEL)),Darwin)
LDFLAGS := -Wall -fPIC -shared -flat_namespace -undefined suppress 
else
LDFLAGS := -Wall -fPIC -shared -undefined 
endif

default: all ;

all: $(LIB_TARGETS) ;

install: all
	install -m 755 $(LIB_TARGETS) $(LIB_DIR)

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

libOgreManager.o: libOgreManager.cpp
	$(CPP) -c -o $@ $(CFLAGS) $< 

libOgreManager.so: CFLAGS += -fPIC $(shell pkg-config --cflags OGRE)
libOgreManager.so: LFLAGS += $(shell pkg-config --libs OGRE)
libOgreManager.so: libOgreManager.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) -o $@
