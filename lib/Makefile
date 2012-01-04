include ../Makedefs

LIB_TARGETS := libymir_core.so 

CFLAGS += -I../packages/gen_cnode/include
CFLAGS += -I$(ERL_INTERFACE_INCLUDE)

ifeq ($(strip $(KERNEL)),Darwin)
LDFLAGS := -Wall -fPIC -rdynamic -shared -flat_namespace
else
LDFLAGS := -Wall -fPIC -rdynamic -shared
endif

default: all ;

all: $(LIB_TARGETS) ;

install: all
	install -m 644 -D $(LIB_TARGETS) $(LIB_DIR)

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

OgreEventListener.o: OgreEventListener.cpp OgreEventListener.h
	$(CPP) -c -o $@ $(CFLAGS) $<

Object.o: Object.cpp Object.h
	$(CPP) -c -o $@ $(CFLAGS) $<

MyGUIObject.o: MyGUIObject.cpp MyGUIObject.h
	$(CPP) -c -o $@ $(CFLAGS) $<

OgreObject.o: CFLAGS += $(shell pkg-config --cflags OGRE MYGUI)
OgreObject.o: OgreObject.cpp OgreObject.h
	$(CPP) -c -o $@ $(CFLAGS) $<

Core.o: Core.cpp Core.h
	$(CPP) -c -o $@ $(CFLAGS) $<

EventManager.o: EventManager.cpp EventManager.h
	$(CPP) -c -o $@ $(CFLAGS) $< 

libymir_core.o: libymir_core.cpp
	$(CPP) -c -o $@ $(CFLAGS) $< 

libymir_core.so: CFLAGS += -fPIC -I$(INC_DIR) -I$(ERL_INTERFACE_INCLUDE)  $(shell pkg-config --cflags OGRE OIS MYGUI)
libymir_core.so: LFLAGS += -L$(ERL_INTERFACE_LIB) -lei -L$(LIB_DIR) $(shell pkg-config --libs OGRE OIS MYGUI)
libymir_core.so: Object.o OgreObject.o MyGUIObject.o OgreEventListener.o Core.o EventManager.o libymir_core.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) -lMyGUI.OgrePlatform -o $@
