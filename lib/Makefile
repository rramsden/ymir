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
	install -m 644 -D $(LIB_TARGETS) $(LIB_DIR)

clean:
	rm -f ./*.o
	rm -f ./*.*~

clobber: clean
	rm -f $(LIB_TARGETS)

OgreEventListener.o: OgreEventListener.cpp OgreEventListener.h
	$(CPP) -c -o $@ $(CFLAGS) $<

YmirObject.o: YmirObject.cpp YmirObject.h
	$(CPP) -c -o $@ $(CFLAGS) $<

MyGUIObject.o: MyGUIObject.cpp MyGUIObject.h
	$(CPP) -c -o $@ $(CFLAGS) $<

OgreObject.o: OgreObject.cpp OgreObject.h
	$(CPP) -c -o $@ $(CFLAGS) $<

OgreManager.o: OgreManager.cpp OgreManager.h
	$(CPP) -c -o $@ $(CFLAGS) $<

EventManager.o: EventManager.cpp EventManager.h
	$(CPP) -c -o $@ $(CFLAGS) $< 

libOgreManager.o: libOgreManager.cpp
	$(CPP) -c -o $@ $(CFLAGS) $< 

libOgreManager.so: CFLAGS += -fPIC -I$(INC_DIR) -I$(ERL_INTERFACE_INCLUDE)  $(shell pkg-config --cflags OGRE OIS MYGUI)
libOgreManager.so: LFLAGS += -L$(ERL_INTERFACE_LIB) -lei -L$(LIB_DIR) $(shell pkg-config --libs OGRE OIS MYGUI)
libOgreManager.so: YmirObject.o MyGUIObject.o OgreObject.o OgreEventListener.o OgreManager.o EventManager.o libOgreManager.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) --whole-archive -lMyGUI.OgrePlatform -o $@
