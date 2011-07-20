#include "OgreManager.h"

#include <errno.h>

OgreManager::OgreManager() :
    root(NULL),
    log(NULL),
    window(NULL),
    rendering(false),

    em(NULL),
    viewport(NULL),
    scene(NULL),
    channel(0),
    queue(NULL)
{

}

OgreManager::~OgreManager(){

}

void OgreManager::logNormal( string msg ){

    if( log ){
        log->logMessage(msg, Ogre::LML_NORMAL);
    } else {
        fprintf(stderr, "INFO:  %s\n", msg.c_str());
    }
}

void OgreManager::logCritical( string msg ){

    if( log ){
        log->logMessage(msg, Ogre::LML_CRITICAL);
    } else {
        fprintf(stderr, "CRITICAL:  %s\n", msg.c_str());
    }
}


int OgreManager::start(string title, string plugin, string config, string logFile){
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

    if( !queue ){
        queue = root->getWorkQueue();

        //Register OgreManager channel
        channel = queue->getChannel("OgreManager");

        printf("CHANNEL! %d\n", channel);
    }

    if( !scene ){
        scene = root->createSceneManager(ST_GENERIC);
    
        scene->setAmbientLight(ColourValue(0.5,0.5,0.5));
    }


    logNormal("Initializing WINDOW!");

    if( !window ){
        window = root->initialise( true, title );

        //Bootstrap CEGUI
        CEGUI::OgreRenderer::bootstrapSystem();
    }

    //Capture input on main window
    logNormal("INPUT SETUP!");
    if( !em ){
        em = EventManager::getSingletonPtr();
        em->initialise(window);
   
        /*if( em->getMouse() ){ 
            em->getMouse()->setEventCallback(em);
        }

        if( em->getKeyboard() ){
            em->getKeyboard()->setEventCallback(em);
        }*/

        root->addFrameListener(em);
    }

    TextureManager::getSingleton().setDefaultNumMipmaps(5);

    logNormal("After init!");
 
    start_exit:
    return rc;
}

void OgreManager::stop(){
   
    if( root ){
        root->shutdown();
        
        if( em ){
            delete em;
        }
        
        if( window ){
            root->detachRenderTarget(window);
        
            CEGUI::OgreRenderer::destroySystem();
        }

        delete root;
    }

    root = NULL;
    scene = NULL;
    window = NULL;
    em = NULL;
    rendering = false;
}

void OgreManager::addResourceLocation( string path, 
                                       string type, 
                                       string group, 
                                       bool recurse = false )
{

    if( root ){
        root->addResourceLocation(path, type, group, recurse);
    }
}

void OgreManager::addEventListener(OgreEventListener* listener){
    
    logNormal("YEAH??");
    
    if( listener && em){
        
        logNormal("Adding handler for keyboard and mouse!");
        em->addEventListener(listener, "Events");
    }
}

void OgreManager::initialiseAllResourceGroups() {

    if( root ){
        ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
    }
}

void OgreManager::addObject( OgreObject* object ){

    if( !scene ){
        logCritical("No scene active!  Unable to add a camera node!"); 
        return;
    }

    object->addToScene(scene);

    delete object;

    /*if( rendering ){ //If rendering, add camera creation to work queues. 

        //<<HERE>> TODO}
        object->addToScene(scene);
    } else { //Otherwise, safe to manually add the camera

        object->addToScene(scene);
   }*/
}

void OgreManager::removeObject( const String& uuid, const OgreObjectType& type ){

    if( !scene ){


    }

    if( rendering ){
        //<<HERE>> TODO
    } else {
    
        /*switch(type) {

            case OBJECT_CAMERA:

                break;

            case OBJECT_LIGHT:

                break;

            case OBJECT_ENTITY:

                break;

            default:

                break;
        }*/   
    }
}

void OgreManager::setViewport( const String& name ){
    Camera* temp = NULL;

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

void OgreManager::windowClosed(RenderWindow* rw){

    rendering = false;
}

int OgreManager::renderStart(){
    int rc = 0;

    if( !root ){
        rc = -EINVAL;
		goto exit;
    }

    rendering = true;
    window->setVisible(true);

    logNormal("RENDERING");
    root->startRendering();

    rendering = false;

    exit:
    return rc;
}

void OgreManager::renderStop(){
    em->setRendering(false);
}

