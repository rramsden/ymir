#include "Core.h"
#include "ObjectFactory.h"

#include <errno.h>

using namespace MyGUI;
using namespace Ymir;

namespace Ymir {

    Core *Core::core;

    Core::Core() :

        mLock(),
        root(NULL),
        log(NULL),
        
        platform(NULL),
        gui(NULL),
    
        window(NULL),
        rendering(false),
    
        em(NULL),
        viewport(NULL),

        mScene(NULL),
        mCCS(NULL),

        mTerrainGlobals(NULL),
        mTerrainGroup(NULL),
        mTerrainPaging(NULL),
        mPageManager(NULL),
        mWorld(NULL),

        mBroadphase(NULL),
        mCollisionConfig(NULL),
        mCollisionDispatcher(NULL),
        mConstraintSolver(NULL),
        mDebugDrawer(NULL),
        mDynamicsWorld(NULL),
        mRigidObjects()

    {
    
    }
    
    Core::~Core(){
    
    }
   
    void Core::logDebug( string msg ){
    
        if( log ){
            log->logMessage(msg, Ogre::LML_TRIVIAL);
        } else {
            fprintf(stderr, "DEBUG:  %s\n", msg.c_str());
        }

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
    
        if( !window ){
            window = root->initialise( true, title );
        }
   
        root->getRenderSystem()->_initRenderTargets();

        //Capture input on main window
        logNormal("Initializing Event Subsystem...");
        if( !em ){
            em = EventManager::getSingletonPtr();
            em->initialise(window);
       
            root->addFrameListener(em);

            //<HERE> Add event listener here?
        }
    
        Ogre::TextureManager::getSingleton().setDefaultNumMipmaps(5);
   
        start_exit:
        return rc;
    }
    
    void Core::stop(){

        if( root ){

            if( mScene ){
                core->logNormal("Destroying scene...");
   
                std::string id = mScene->getName();
                PropList temp = PropList();

                ObjectFactory::destroy(id, Object::Scene, temp);
            }

            if( window ){
                root->detachRenderTarget(window);
            }

            root->shutdown();

            if( em ){
                delete em;
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
  
    void Core::windowClosed(RenderWindow* rw){
    
        rendering = false;
    }
  
    Ogre::RaySceneQuery* Core::createRayQuery(Ogre::Real x, Ogre::Real y){
        
        if( !viewport || !mScene || !window ){
            //TODO: Exception
        }

        Ogre::Real nX = x / window->getWidth();
        Ogre::Real nY = y / window->getHeight();
        Ogre::Camera* cam = viewport->getCamera();

        if( !cam ){
            //TODO: Exception 
        }

        //Create the ray from viewport pov
        Ogre::Ray ray = cam->getCameraToViewportRay(nX, nY);

        //Cast the ray into the scene, gathering up all objects
        //it intersects with (in sorted order)
        Ogre::RaySceneQuery* query = mScene->createRayQuery(ray);

        return query;
    }
    
    void Core::destroyQuery(Ogre::SceneQuery* query){
        if( !mScene ){
            //TODO: Exception
        }

        mScene->destroyQuery(query);
    }

    int Core::ticktock(){


        if( !root || !window || !mDynamicsWorld ){
            return -EINVAL;
        }

        Ogre::WindowEventUtilities::messagePump();  
        
        logNormal("Rendering one!");
        root->renderOneFrame();

        return 0;
    }
    
    void Core::resetTimer(){
        //Clear ogre central timer
        if( root ){
            root->clearEventTimes();
        }
    }

    void Core::create( string& objectID, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props )
    {

        ObjectFactory::create(objectID, type, props);
    }
 
    void Core::update( string& objectID, 
                       Ymir::Object::Type type,
                       Ymir::PropList& props )
    {

        ObjectFactory::update(objectID, type, props);
    }
    
    void Core::destroy( std::string& id,
                        Ymir::Object::Type type, 
                        Ymir::PropList& props )
    {
        ObjectFactory::destroy(id, type, props);
    }

    Ymir::Core* Core::getSingletonPtr( ) {
        if( !core ) {
            core = new Ymir::Core();
        }
     
        return core;
    }

}
