#ifndef CORE_H
#define CORE_H

//OGRE 
#include <OGRE/Ogre.h>
#include <OGRE/OgreLog.h>

//MyGUI
#include <MyGUI.h>
#include <MyGUI_OgrePlatform.h>

//Input System
#include <OIS/OIS.h>

//Event dispatching support
#include "OgreEventListener.h"
#include "EventManager.h"

//Generic Object Support
#include "OgreObject.h"
#include "MyGUIObject.h"

//Object delegation
#include "Task.h"

using namespace std;
using namespace Ogre;

namespace Ymir {
    class Core : public WindowEventListener 
    {
    
        public:

            ~Core();
    
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
            void create( const std::string& uuid, Ymir::Object::Type type, Ymir::PropList& props );
            void update( const std::string& uuid, Ymir::PropList& actions);
            void destroy( const std::string& uuid );

            //Viewport management
            void setViewport( const String& name );
   
            //Scene Management
            void destroyAllObjects();

            //Getters
            Ogre::SceneManager* getScene() { return scene; }

            //void clearScene();

            //OGRE hooks
            //bool frameStarted(const FrameEvent &event);
            void windowClosed(RenderWindow* rw);
   
            static Ymir::Core* getSingletonPtr(); 
        protected:
            Core();

            void setupInputDevices();
            void loadResources();

            void createActual( const string& uuid,
                               Ymir::Object::Type type,
                               Ymir::PropList& props );

            void updateActual( const string& uuid,
                               Ymir::Object::Type type,
                               Ymir::PropList& props );
            
            void destroyActual( const string& uuid,
                                Ymir::Object::Type type,
                                Ymir::PropList& props );
            
            Ogre::Root* root;
            Ogre::Log* log;
            
            MyGUI::OgrePlatform* platform;
            MyGUI::Gui* gui;
            
            RenderWindow* window;
            bool rendering;
    
            EventManager* em;
            Ogre::Viewport* viewport;
            Ogre::SceneManager* scene;

            std::map<std::string, Ymir::Object*> objects;

            static Core* core;
    };

    class CoreTask : public Task {

        typedef void (Ymir::Core::*CoreFP)();

        public:
        CoreTask( Ymir::Core* ptr, CoreFP fun ) : object(ptr), fp(fun) {}
        ~CoreTask(){}

        void run() { ((object)->*(fp))(); }

        protected:
            Ymir::Core* object;
            CoreFP fp;
    };

    class CoreObjectTask : public Task {

        typedef void (Ymir::Core::*CoreFP)( const std::string&, 
                                            Ymir::Object::Type type,
                                            Ymir::PropList& );

        public:
            CoreObjectTask( CoreFP fp, 
                            const std::string& id,
                            Ymir::Object::Type type = Ymir::Object::Invalid,
                            Ymir::PropList props = Ymir::PropList() ) : fp(fp), id(id), type(type), props(props) {}

            ~CoreObjectTask(){}

            void run() { 
                ((Ymir::Core::getSingletonPtr())->*(fp))(id, type, props);
            }

        protected:
            CoreFP fp; 
            std::string id;
            Ymir::Object::Type type;
            Ymir::PropList props;
            
    };
    /*class CoreObjectTask : public Task {
        typedef void (Ymir::Core::*CoreObjectFP) (Ymir::Object*);

        public: 
            CoreObjectTask( Ymir::Core* ptr, 
                            CoreObjectFP fun, 
                            Ymir::Object* obj ) : 
                instance(ptr), 
                fp(fun), 
                object(obj) 
            {       
        
            }

            ~CoreObjectTask(){}

            void run() { ((instance)->*(fp))(object); }

        protected:
            Ymir::Core* instance;
            CoreObjectFP fp;
            Ymir::Object* object;
    };*/

}

#endif
