#include "OgreManager.h"

#include <errno.h>

OgreManager::OgreManager() :
    root(NULL),
    window(NULL),
    running(false),
    input(NULL),
    sceneManager(NULL),
    camera(NULL),
    viewport(NULL),
    luaApp(NULL),
    luaInGame(NULL)
{

}

OgreManager::~OgreManager(){

}

int OgreManager::start(string title, string plugin, string config, string log){
    int rc = 0;

    if( running ){
		rc = -EALREADY;
        goto start_exit;
    }

    if( !root ){
        root = new Root(plugin, config, log);
    }

    if( !root->restoreConfig() && !root->showConfigDialog() ){
		rc = -ENOTSUP;
        goto start_exit;
    }

    printf("Initializing WINDOW!\n");
    if( !window ){
        window = root->initialise( true, title );

        //Bootstrap CEGUI
        CEGUI::OgreRenderer::bootstrapSystem();
    }

    printf("After init!\n");
 
    if( !luaApp ){
        luaApp = lua_open();
    }

    //<<HERE>> Register application level lua functions

    if( !luaInGame ){
        luaInGame = lua_open();
    }

    //<<HERE>> Call into lua splash screen?

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

    if( luaApp ){
        lua_close(luaApp);
    }

    if( luaInGame ){
        lua_close(luaInGame);
    }

    root = NULL;
    window = NULL;
    input = NULL;
   
    luaApp = NULL;
    luaInGame = NULL;

    running = false;
}

int OgreManager::loadModule(string module) {
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

int OgreManager::unloadModule() {
    int rc = 0;

    return rc;
}

bool OgreManager::frameStarted(const FrameEvent &evt){
   
    input->capture();

    if( input->getKeyboard()->isKeyDown(OIS::KC_ESCAPE) ){
        this->renderStop();
    }

    return running;
}

void OgreManager::windowClosed(RenderWindow* rw){

    running = false;
}

int OgreManager::renderStart(){
    int rc = 0;

    if( !root ){
        rc = -EINVAL;
		goto exit;
    }

    //Create rendering window
    //window = root->createRenderWindow(title, width, height, fullscreen);

    //Capture input on main window
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

    printf("INPUT SETUP!\n");
    
    //Register OM as a framelistener and windoweventlistener
    root->addFrameListener(this);
    WindowEventUtilities::addWindowEventListener(window, this);
    
    printf("Framelistener added!\n");
   
    //<<HERE>> Detach splash and render main window

    running = true;
    window->setVisible(true);

    printf("RENDERING\n");
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

    running = false;
}

