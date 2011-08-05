#ifndef OGREMANAGER_H
#define OGREMANAGER_H


//OGRE 
#include <OGRE/Ogre.h>
#include <OGRE/OgreLog.h>

//MyGUI
#include <MyGUI.h>
#include <MyGUI_OgrePlatform.h>

//Input System
#include <OIS/OIS.h>

//Generic Object Support
#include "OgreObject.h"
#include "MyGUIObject.h"

#include "OgreEventListener.h"

#include "EventManager.h"

using namespace std;
using namespace Ogre;

namespace Ymir {
    class OgreManager : public WindowEventListener 
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
            void initialiseMyGUI(std::string& config);

            void addEventListener(OgreEventListener* event);
    
            //Setup and tear down of input and rendering
            int renderStart();
            void renderStop();
    
            //Object management
            template<typename T>
            void add(Ymir::OgreObject<T>* object);

            template<typename T>
            void add(Ymir::MyGUIObject<T>* object);

            template<typename T>
            void update(Ymir::OgreObject<T>* object);

            template<typename T>
            void update(Ymir::MyGUIObject<T>* object);

            void removeObject( const String& uuid, const ObjectType& type );
    
            //Viewport management
            void setViewport( const String& name );
    
            //OGRE hooks
            //bool frameStarted(const FrameEvent &event);
            void windowClosed(RenderWindow* rw);
    
        protected:
            void setupInputDevices();
            void loadResources();
    
            Ogre::Root* root;
            Ogre::Log* log;
            
            MyGUI::OgrePlatform* platform;
            MyGUI::Gui* gui;
            
            RenderWindow* window;
            bool rendering;
    
            EventManager* em;
            Ogre::Viewport* viewport;
            Ogre::SceneManager* scene;
            Ogre::uint16 channel;
            Ogre::WorkQueue* queue;
    };

    template<typename T> 
    void OgreManager::add( OgreObject<T>* object ){
    
        if( !scene ){
            logCritical("No scene active!  Unable to add object!"); 
            return;
        }
    
        printf("Calling OBJECT ADD!\n");
    
        object->add(scene);
    
        //delete object;
    
        /*if( rendering ){ //If rendering, add camera creation to work queues. 
    
            //<<HERE>> TODO}
            object->addToScene(scene);
        } else { //Otherwise, safe to manually add the camera
    
            object->addToScene(scene);
       }*/
    }
 
    template<typename T>
    void OgreManager::add( MyGUIObject<T>* object ){

        if( !scene || !gui ){
            logCritical("MyGUI not initialised!  Unable to add gui object!");
            return;
        }

        logCritical("In add MYGUI object!");

        T* widget = object->add(gui);

        if( !widget ){
            logCritical("NULL!!");
        }

        logCritical("Monitoring MYGUI object!");
        em->monitor(widget);
    }


    template<typename T>
    void OgreManager::update( OgreObject<T>* object ){
    
        if( !scene ){
            logCritical("No scene active!  Unable to add object!"); 
            return;
        }
    
        object->update(scene);
    
        delete object;
    }
    
    template<typename T>
    void OgreManager::update(MyGUIObject<T>* object){

        if( !scene || !gui ){
            logCritical("MyGUI not intialised! Unable to update gui object!");
            return;
        }

        object->update(gui);



        delete object;
    }
}

#endif
