include ../Makedefs

LIB_TARGETS := libOgreManager.so 

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

OgreManager.o: OgreManager.cpp OgreManager.h
	$(CPP) -c -o $@ $(CFLAGS) $<

InputManager.o: InputManager.cpp InputManager.h
	$(CPP) -c -o $@ $(CFLAGS) $< 

DotSceneLoader.o: DotSceneLoader.cpp DotSceneLoader.h
	$(CPP) -c -o $@ $(CFLAGS) $< 

libOgreManager.o: libOgreManager.cpp
	$(CPP) -c -o $@ $(CFLAGS) $< 

libOgreManager.so: CFLAGS += -fPIC -I$(ERL_INTERFACE_INCLUDE) $(shell pkg-config --cflags OGRE CEGUI OIS lua5.1)
libOgreManager.so: LFLAGS += -L$(ERL_INTERFACE_LIB) -lei $(shell pkg-config --libs OGRE CEGUI OIS tinyxml lua5.1)
libOgreManager.so: OgreManager.o DotSceneLoader.o InputManager.o libOgreManager.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) -o $@
