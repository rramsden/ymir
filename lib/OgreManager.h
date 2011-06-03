#ifndef OGREMANAGER_H
#define OGREMANAGER_H

#include <OGRE/Ogre.h>
#include <OIS/OIS.h>

//Lua Support
extern "C" {
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}

#include "DotSceneLoader.h"
#include "InputManager.h"

using namespace std;
using namespace Ogre;

class OgreManager : public WindowEventListener, 
                    public FrameListener 
{

    public:
        OgreManager();
        ~OgreManager();

        //Setup and tear down of ogre root
        int start(string plugins, string config, string log);
        void stop();

        //Setup and tear down of input and rendering
        int renderStart( string title="OGRE",
                         unsigned int width=1024, 
                         unsigned int height=768,
                         bool fullscreen=true );
        void renderStop();
 
        //Scene management 
        int sceneStart(int id, char* dotscene_file);
        int sceneEnd(int id);
        int sceneSwap(int from, int to);

        //Object management
        int objectAdd(int sceneId, int objectId, char* object_file);
        int objectRemove( int sceneId, int objectId);

        //int objectUpdate( int id, lkjaf ); 

        //OGRE hooks
        bool frameStarted(const FrameEvent &event);
        void windowClosed(RenderWindow* rw);

    protected:
        void setupInputDevices();
        void loadResources();

        Root* root;
        RenderWindow* window;
        bool running;

        InputManager* input;
        SceneManager* sceneManager;

        //Lua domains
        lua_State* luaApp;     //Lua scripting support overriding OM behaviours
        lua_State* luaInGame;  //Lua scripting support for in-game events
};


#endif
