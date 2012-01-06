#include "Core.h"

#include <errno.h>

using namespace MyGUI;
using namespace Ymir;

namespace Ymir {

    Core *Core::core;

    Core::Core() :
        root(NULL),
        log(NULL),
        
        platform(NULL),
        gui(NULL),
    
        window(NULL),
        rendering(false),
    
        em(NULL),
        viewport(NULL),
        scene(NULL),

        objects()
    {
    
    }
    
    Core::~Core(){
    
    }
    
    void Core::logNormal( string msg ){
    
        if( log ){
            log->logMessage(msg, Ogre::LML_NORMAL);
        } else {
            fprintf(stderr, "INFO:  %s\n", msg.c_str());
        }
    }
    
    void Core::logCritical( string msg ){
    
        if( log ){
            log->logMessage(msg, Ogre::LML_CRITICAL);
        } else {
            fprintf(stderr, "CRITICAL:  %s\n", msg.c_str());
        }
    }
    
    
    int Core::start( string title, 
                     string plugin, 
                     string config, 
                     string logFile){
        int rc = 0;
    
        if( rendering ){
    		rc = -EALREADY;
            goto start_exit;
        }
    
        if( !root ){
            root = new Root(plugin, config, logFile);
        }
    
        if( !log ){
            log = Ogre::LogManager::getSingleton().getDefaultLog();
        }
            
        if( !root->restoreConfig() && !root->showConfigDialog() ){
    		rc = -ENOTSUP;
            goto start_exit;
        }
    
        if( !scene ){
            scene = root->createSceneManager(ST_GENERIC);
        
            scene->setAmbientLight(ColourValue(0.5,0.5,0.5));
        }
    
        if( !window ){
            window = root->initialise( true, title );
        }
    
        //Capture input on main window
        logNormal("Initializing Event Subsystem...");
        if( !em ){
            em = EventManager::getSingletonPtr();
            em->initialise(window);
       
            root->addFrameListener(em);
        }
    
        Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);
    
        start_exit:
        return rc;
    }
    
    void Core::stop(){

        if( rendering ){      
            renderStop();
        }

        if( root ){
            root->shutdown();
            
            if( em ){
                delete em;
            }
            
            if( window ){
                root->detachRenderTarget(window);
            }
    
            if( gui ){
                
                gui->shutdown();
                delete gui;
            }
    
            if( platform ){
            
                platform->shutdown();
                delete platform;
            }
    
    
            delete root;
        }
    
        root = NULL;
        log = NULL;
        platform = NULL;
        gui = NULL;
        scene = NULL;
        window = NULL;
        em = NULL;
        rendering = false;
    }
    
    void Core::addResourceLocation( string path, 
                                           string type, 
                                           string group, 
                                           bool recurse = false )
    {
    
        if( root ){
            root->addResourceLocation(path, type, group, recurse);
        }
    }
    
    void Core::addEventListener(OgreEventListener* listener){
        
        logNormal("YEAH??");
        
        if( listener && em){
            
            logNormal("Adding handler for keyboard and mouse!");
            em->addEventListener(listener, "Events");
        }
    }
    
    void Core::initialiseAllResourceGroups() {
    
        if( root ){
            ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
        }
    }
  
    void Core::initialiseMyGUI(string& config){
        if( root ){
            logNormal("Initializing MyGUI Subsystem...");
            if( !platform ){
       
                platform = new OgrePlatform();
                platform->initialise(window, scene);
            }
    
            if( !gui ){
                gui = new Gui();
                gui->initialise();
          }

        }
    }

    void Core::setViewport( const String& name ){
        Ogre::Camera* temp = NULL;
    
        if( !window ){ 
            logCritical("No render window active!  Unable to add camera!"); 
            return;
        }
    
        if( rendering ){
            //<<HERE>> TODO
        } else {
       
            temp = scene->getCamera(name);
    
            if( viewport ){
                //<<HERE>> TODO
            } else {
                viewport = window->addViewport(temp);
            }
        }
    }
    
    void Core::windowClosed(RenderWindow* rw){
    
        rendering = false;
    }
    
    int Core::renderStart(){
        int rc = 0;
    
        if( !root ){
            rc = -EINVAL;
    		goto exit;
        }
    
        rendering = true;
        window->setVisible(true);
    
        root->startRendering();
    
        rendering = false;
    
        exit:
        return rc;
    }
    
    void Core::renderStop(){
        em->setRendering(false);
    }

    void Core::create( const string& id, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props ){
    
        if( rendering ){
            em->queueTask(new CoreObjectTask(&Core::createActual, id, type, props));
        } else {
            CoreObjectTask(&Ymir::Core::createActual, id, type, props).run();
        }
    }
 
    void Core::update( const string& id, 
                       Ymir::PropList& props ){
    
        if( rendering ){
            em->queueTask(new CoreObjectTask(&Core::updateActual, id, Ymir::Object::Invalid, props));
        } else { 
            CoreObjectTask(&Ymir::Core::updateActual, id, Ymir::Object::Invalid, props).run();
        }
    }
    
    void Core::destroy( const string& id ){
        if( rendering ){
            em->queueTask(new CoreObjectTask(&Core::destroyActual, id));
        } else {
            CoreObjectTask(&Ymir::Core::destroyActual, id).run();
        }
    }

    void Core::createActual( const string& uuid,
                             Ymir::Object::Type type,
                             Ymir::PropList& props )
    {
        Ymir::Object* object = NULL;

        logNormal("CREATE ACTUAL!\n");

        switch( type ){
    
            case Ymir::Object::Camera:
                object = new Ymir::Camera(uuid);
                break;

            case Ymir::Object::Light:
                object = new Ymir::Light(uuid);
                break;

            case Ymir::Object::Entity:
                object = new Ymir::Entity(uuid);
                break;

            case Ymir::Object::Button:
                object = new Ymir::Button(uuid);
                break;

            case Ymir::Object::Window:
                object = new Ymir::Window(uuid);
                break;
                
            default:
                logNormal("Invalid object type detected!");
                return;
        }

        logNormal("Calling object create!");

        //Tell the object to create itself
        object->create(props);

        //Add the object to the map of tracked objects
        objects.insert( std::pair<std::string, Ymir::Object*>(uuid, object) );
    }

    void Core::updateActual( const string& uuid,
                             Ymir::Object::Type type,
                             Ymir::PropList& props ){
        std::map<std::string, Ymir::Object*>::iterator it = objects.find(uuid);

        if( it != objects.end() ){
            it->second->update(props);
        }
    }

    void Core::destroyActual( const string& uuid,
                              Ymir::Object::Type type,
                              Ymir::PropList& props ){
        std::map<std::string, Ymir::Object*>::iterator it = objects.find(uuid);

        if( it != objects.end() ){
            
            it->second->destroy();
            objects.erase(it);
        }
    }

    Ymir::Core* Core::getSingletonPtr( ) {
        if( !core ) {
            core = new Ymir::Core();
        }
     
        return core;
    }

}
