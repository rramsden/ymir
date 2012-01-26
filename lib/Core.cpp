#include "Core.h"
#include "ObjectFactory.h"

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
        mActiveScene(NULL),
        mActivePageManager(NULL),

        scenes()
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
   
        //Initialize ogre root 
        if( !root ){
            root = new Root(plugin, config, logFile);
        }
    
        if( !log ){
            log = Ogre::LogManager::getSingleton().getDefaultLog();
        }
        
        //Ensure Ogre is configured    
        if( !root->restoreConfig() && !root->showConfigDialog() ){
    		rc = -ENOTSUP;
            goto start_exit;
        }
    
        //<<HERE>> This needs to be taken out into a new function.
        //New bootstrap process will be: start, load scenes,
        //activateScene (MyGui comes with this?)
        /*if( !mActiveScene ){
            mActiveScene = root->createSceneManager(ST_GENERIC);
        
            mActiveScene->setAmbientLight(ColourValue(0.5,0.5,0.5));
        }*/
    
        if( !window ){
            window = root->initialise( true, title );
        }
    
        //Capture input on main window
        logNormal("Initializing Event Subsystem...");
        if( !em ){
            em = EventManager::getSingletonPtr();
            em->initialise(window);
       
            root->addFrameListener(em);

            //<HERE> Add event listener here?
        }
    
        Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);
   
        //Setup terrain page manager
        //mPageManager = OGRE_NEW Ogre::PageManager();
        //mTerrainPaging = OGRE_NEW Ogre::TerrainPaging(mPageManager); 

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
        mActiveScene = NULL;
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
        
        if( listener && em){
            
            logNormal("Adding handler for keyboard and mouse!");
            em->addEventListener(listener, "Events");
        }
    }
    
    void Core::initialiseAllResourceGroups() {
    
        if( root ){
            Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
        }
    }
  
    void Core::initialiseMyGUI(string& config){
        if( root ){
            logNormal("Initializing MyGUI Subsystem...");
            if( !platform ){
       
                platform = new OgrePlatform();
                platform->initialise(window, mActiveScene);
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
       
            temp = mActiveScene->getCamera(name);
            if( !temp ){
                //<<HERE>> TODO
            }

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

    Ogre::SceneManager* Core::findScene( const string& scene ){
        std::map<string, Ogre::SceneManager*> it = scenes.find(scene);

        if( it == scenes.end() ){
            return NULL;
        } else {
            it->second();
        }
    }

    void Core::create( const string& sceneID,
                       const string& objectID, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props )
    {
        Ogre::SceneManager* scene = findScene(sceneID);
        Ymir::ObjectTask task = Ymir::ObjectTask::create(scene, objectID, type, props):

        if( rendering && scene == activeScene ){
            em->queueTask( task );
        } else if( scene ){
            task.run();
        }
    }
 
    void Core::update( const string& sceneID,
                       const string& objectID, 
                       Ymir::PropList& props )
    {
        Ogre::SceneManager* scene = findScene(sceneID);
        Ymir::ObjectTask task = Ymir::ObjectTask::update(scene, objectID, props);

        if( rendering && scene == activeScene ){
            em->queueTask(task);
        } else if( scene ) { 
            task.run();
        }
    }
    
    void Core::destroy( const string& sceneID,
                        const string& objectID )
    {
        Ogre::SceneManager* scene = findScene(sceneID);
        Ymir::ObjectTask task = Ymir::ObjectTask::destroy(scene, objectID);

        if( rendering && scene == activeScene ){
            em->queueTask(task);
        } else {
            task.run();  
        }
    }

    /*void Core::createActual( const string& scene,
                             const string& uuid,
                             Ymir::Object::Type type,
                             Ymir::PropList& props )
    {
        std::map<std::string, Ogre::SceneManager*>::iterator it = scenes.find(scene);
        Ymir::Object* object = NULL;

        logNormal("CREATE ACTUAL!\n");

        if( it != scenes.end() ){
            Ymir::ObjectFactory::create<type>(it->second, uuid, props);
        }

        switch( type ){
   
            case Ymir::Object::Scene:

                break;

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

    void Core::updateActual( const string& scene,
                             const string& uuid,
                             Ymir::Object::Type type,
                             Ymir::PropList& props )
    {
        std::map<std::string, Ogre::SceneManager*>::iterator it = scenes.find(scene); 

        if( it != objects.end() ){
            Ymir::ObjectFactory::update(it->second, uuid, props);
        }
    }

    void Core::destroyActual( const string& scene,
                              const string& uuid,
                              Ymir::Object::Type type,
                              Ymir::PropList& props ){
        std::map<std::string, Ogre::SceneManager*>::iterator it = scenes.find(scene);

        if( it != objects.end() ){
            Ymir::ObjectFactory::destroy(it->second, uuid);
        }
    }*/

    Ymir::Core* Core::getSingletonPtr( ) {
        if( !core ) {
            core = new Ymir::Core();
        }
     
        return core;
    }

}
