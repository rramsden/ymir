#ifndef OGREMANAGER_H
#define OGREMANAGER_H


//OGRE 
#include <OGRE/Ogre.h>
#include <OGRE/OgreLog.h>

//CE GUI
#include <CEGUI.h>
#include <RendererModules/Ogre/CEGUIOgreRenderer.h>

//Input System
#include <OIS/OIS.h>
#include "InputManager.h"

//Generic Object Support
#include "OgreObject.h"
#include "OgreEvent.h"

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

        void logNormal( string msg );
        void logCritical( string msg );

        //Resource Management
        void addResourceLocation( string path, 
                                  string type, 
                                  string group, 
                                  bool recurse );

        void initialiseAllResourceGroups();

        void addEventHandler(OgreEvent* event);

        //Setup and tear down of input and rendering
        int renderStart();
        void renderStop();

        //Object management
        void createObject(OgreObject* object);
        void destroyObject( const String& uuid, const OgreObjectType& type );

        //Viewport management
        void setViewport( const String& name );

        //OGRE hooks
        bool frameStarted(const FrameEvent &event);
        void windowClosed(RenderWindow* rw);

    protected:
        void setupInputDevices();
        void loadResources();

        Ogre::Root* root;
        Ogre::Log* log;
        RenderWindow* window;
        bool rendering;

        InputManager* input;
        Ogre::Viewport* viewport;
        Ogre::SceneManager* scene;
};


#endif
