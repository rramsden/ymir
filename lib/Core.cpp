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

        mScene(NULL),
        mTerrainGroup(NULL),
        mTerrainPaging(NULL),
        mPageManager(NULL),
        mWorld(NULL)

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
        if( !mScene ){
            mScene = root->createSceneManager(ST_GENERIC);
        
            mScene->setAmbientLight(ColourValue(0.5,0.5,0.5));
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
        mScene = NULL;
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
                platform->initialise(window, mScene);
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
       
            temp = mScene->getCamera(name);
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

    void Core::create( string& objectID, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props )
    {

        if( rendering ){
            em->queueTask(new Ymir::FactoryTask( FactoryTask::Create,
                                                 objectID, type, props));
        } else {
            Ymir::FactoryTask(FactoryTask::Create, objectID, type, props).run();
        } 
    }
 
    void Core::update( string& objectID, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props )
    {

        if( rendering ){
            em->queueTask(new Ymir::FactoryTask( FactoryTask::Update,
                                                 objectID, type, props));
        } else {
            Ymir::FactoryTask(FactoryTask::Update, objectID, type, props).run();
        } 
    }
    
    void Core::destroy( std::string& id,
                        Ymir::Object::Type type, 
                        Ymir::PropList& props )
    {

        if( rendering ){
            em->queueTask(new Ymir::FactoryTask( FactoryTask::Destroy,
                                                 id, type, props));
        } else {
            Ymir::FactoryTask(FactoryTask::Destroy, id, type, props).run();
        } 
    }

    Ymir::Core* Core::getSingletonPtr( ) {
        if( !core ) {
            core = new Ymir::Core();
        }
     
        return core;
    }

}
