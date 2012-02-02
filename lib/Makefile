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

DecodeBasic.o: DecodeBasic.cpp DecodeBasic.h
	$(CPP) -c -o $@ $(CFLAGS) $<

OgreEventListener.o: OgreEventListener.cpp OgreEventListener.h
	$(CPP) -c -o $@ $(CFLAGS) $<

ObjectBlueprint.o: ObjectBlueprint.cpp ObjectBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

OgreBlueprint.o: OgreBlueprint.cpp OgreBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

EntityBlueprint.o: EntityBlueprint.cpp EntityBlueprint.h OgreBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

LightBlueprint.o: LightBlueprint.cpp LightBlueprint.h OgreBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

CameraBlueprint.o: CameraBlueprint.cpp CameraBlueprint.h NodeBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

TerrainBlueprint.o: TerrainBlueprint.cpp TerrainBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

ButtonBlueprint.o: ButtonBlueprint.cpp ButtonBlueprint.h MyGUIBlueprint.h
	$(CPP) -c -o $@ $(CFLAGS) $<

ObjectFactory.o: ObjectFactory.cpp ObjectFactory.h
	$(CPP) -c -o $@ $(CFLAGS) $<

Core.o: Core.cpp Core.h
	$(CPP) -c -o $@ $(CFLAGS) $<

EventManager.o: EventManager.cpp EventManager.h
	$(CPP) -c -o $@ $(CFLAGS) $< 

libymir_core.o: libymir_core.cpp
	$(CPP) -c -o $@ $(CFLAGS) $< 

libymir_core.so: CFLAGS += -fPIC -I$(INC_DIR) -I$(ERL_INTERFACE_INCLUDE)  $(shell pkg-config --cflags OGRE-Terrain OIS MYGUI)
libymir_core.so: LFLAGS += -L$(ERL_INTERFACE_LIB) -lei -L$(LIB_DIR) $(shell pkg-config --libs OGRE-Terrain OIS MYGUI)
libymir_core.so: DecodeBasic.o ObjectBlueprint.o OgreBlueprint.o EntityBlueprint.o LightBlueprint.o CameraBlueprint.o TerrainBlueprint.o ButtonBlueprint.o ObjectFactory.o OgreEventListener.o Core.o EventManager.o libymir_core.o
	$(CPP) $(LDFLAGS) $^ $(LFLAGS) -lMyGUI.OgrePlatform -o $@
