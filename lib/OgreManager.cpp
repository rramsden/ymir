#include "OgreManager.h"

#include <errno.h>

OgreManager::OgreManager() :
    root(NULL),
    log(NULL),
    window(NULL),
    rendering(false),
    input(NULL),
    viewport(NULL),
    scene(NULL)
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
    if( !input ){
        input = InputManager::getSingletonPtr();
        input->initialise(window);
   
        if( input->getMouse() ){ 
            input->getMouse()->setEventCallback(input);
        }

        if( input->getKeyboard() ){
            input->getKeyboard()->setEventCallback(input);
        }
    }

    TextureManager::getSingleton().setDefaultNumMipmaps(5);

    logNormal("After init!");
 
    start_exit:
    return rc;
}

void OgreManager::stop(){
   
    if( root ){
        root->shutdown();
        
        if( input ){
            delete input;
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
    input = NULL;
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

void OgreManager::addEventHandler(OgreEvent* handler){
    
    logNormal("YEAH??");
    
    if( handler && input){
        
        logNormal("Adding handler for keyboard and mouse!");
        input->addKeyListener(handler, "Events");
        input->addMouseListener(handler, "Events");
    }
}

void OgreManager::initialiseAllResourceGroups() {

    if( root ){
        ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
    }
}

void OgreManager::createObject( OgreObject* object ){

    if( !scene ){
        logCritical("No scene active!  Unable to add a camera node!"); 
        return;
    }

    if( rendering ){ //If rendering, add camera creation to work queues. 

        //<<HERE>> TODO} 
    } else { //Otherwise, safe to manually add the camera

        object->addToScene(scene);
   }
}

void OgreManager::destroyObject( const String& uuid, const OgreObjectType& type ){

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


/*int OgreManager::loadModule(string module) {
    int rc = 0;
    String scene = "test.scene";
    String group = "Default";

    if( module == "" ){
        module = ".";
    }

    printf("Path = %s\n", module.c_str() );

    //<<HERE>> Do something better?
    root->addResourceLocation( module + "/Models", "FileSystem", group );
    root->addResourceLocation( module + "/Terrain", "FileSystem", group );
    root->addResourceLocation( module + "/Materials", "FileSystem", group );
    root->addResourceLocation( module + "/Scripts", "FileSystem", group );
    root->addResourceLocation( module + "/Programs", "FileSystem", group );
    root->addResourceLocation( module + "/Scenes", "FileSystem", group );

    //<<HERE>> Vector with a scene manager for each?
    printf("0\n");
    sceneManager = root->createSceneManager(ST_GENERIC);
    printf("1\n");
    
    camera = sceneManager->createCamera("Camera");
    camera->setPosition(Ogre::Vector3(0,0,80));
    camera->lookAt(Ogre::Vector3(0,0,-300));
    camera->setNearClipDistance(5);

    viewport = window->addViewport(camera);

    TextureManager::getSingleton().setDefaultNumMipmaps(5);

    ResourceGroupManager::getSingleton().initialiseAllResourceGroups();

    printf("Before parseDotScene!!\n");

    Ogre::Entity* head = sceneManager->createEntity("Head","ogrehead.mesh");
    Ogre::SceneNode* headNode = sceneManager->getRootSceneNode()->createChildSceneNode();

    headNode->attachObject(head);

    sceneManager->setAmbientLight(Ogre::ColourValue(0.5, 0.5, 0.5));

    Ogre::Light* l = sceneManager->createLight("MainLight");
    l->setPosition(20,80,50);

    printf("After parseDotScene!\n");

    return rc ;
}

int OgreManager::sceneRegister(string uuid, string type){

    if( scenes.end() == scenes.find(uuid) ){
        return -EALREADY;
    }

    scenes.insert(pair<string, Ogre::SceneManager*>(uuid, root->createSceneManager(type)));
    return 0;
}

void OgreManager::sceneOpen(string uuid, string cameraName){
    std::map<string, SceneManager*>::iterator iter = NULL;

    if( root && window && (iter = scenes.find(uuid)) != scenes.end() ){
   
        //<<HERE>> Fade to black or something?
        if( scene ){

        }

        //Locate the camera intended opening camera
        if( !(Camera* camera = scene->getCamera(cameraName)) ){
            //<<HERE>> camera not found!
        }

        //Update active scene
        scene = iter.second;

        //Update viewport 
        viewport = window->addViewport(camera);
    }
}

void sceneClose(){


}*/

bool OgreManager::frameStarted(const FrameEvent &evt){
   
    input->capture();

    if( input->getKeyboard()->isKeyDown(OIS::KC_ESCAPE) ){
        this->renderStop();
    }

    return rendering;
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

    //Register OM as a framelistener and windoweventlistener
    root->addFrameListener(this);
    WindowEventUtilities::addWindowEventListener(window, this);
    
    logNormal("Framelistener added!");
   
    //<<HERE>> Detach splash and render main window

    rendering = true;
    window->setVisible(true);

    logNormal("RENDERING");
    root->startRendering();

    exit:
    return rc;
}

/*int OgreManager::renderStart(){
    int rc = 0;
    Timer* timer = NULL;
    unsigned long delta = 0, last = 0;
    float delta_s = 0;


    if( !root || !window || !keyboard ){
        rc = -EINVAL;
        goto render_exit;
    }

    //Reset the timer for sync
    timer = root->getTimer();
    timer->reset();
    
    while( !window->isClosed() ){
        unsigned long current = timer->getMilliseconds();
        
        if( (delta = (current - last)) <= 0 ){
            continue;
        }
        
        last = current;
        delta_s = 0.001f * float(delta);

        //Capture the keyboard events
        keyboard->capture();
        if( keyboard->isKeyDown(OIS::KC_ESCAPE) ){
            break;
        }

        window->update(false);
        window->swapBuffers(true);

        root->renderOneFrame();

        WindowEventUtilities::messagePump();
    }

    renderStop();

    render_exit:
    return rc;
}*/

void OgreManager::renderStop(){

    //Queue up the end of rendering
    //if( root ){
    //    root->removeFrameListener(this);
    //    WindowEventUtilities::removeWindowEventListener(window, this);
    //}
    if( input ){
        delete input;
        input = NULL;
    }

    rendering = false;
}

