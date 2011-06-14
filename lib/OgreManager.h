#ifndef OGREMANAGER_H
#define OGREMANAGER_H

#include <OGRE/Ogre.h>
#include <CEGUI.h>
#include <RendererModules/Ogre/CEGUIOgreRenderer.h>
#include <OIS/OIS.h>

//Lua Support
extern "C" {
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}

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
        int start(string title, string plugins, string config, string log);
        void stop();

        //Setup and tear down of input and rendering
        int renderStart();
        void renderStop();

        //Module management
        int loadModule( string module );
        int unloadModule( );

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
        Camera* camera;
        Viewport* viewport;

        //Lua domains
        lua_State* luaApp;     //Lua scripting support overriding OM behaviours
        lua_State* luaInGame;  //Lua scripting support for in-game events
};


#endif
