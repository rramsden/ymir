#include <stdio.h>

#include <OGRE/Ogre.h>
#include <OIS/OIS.h>

#include "gen_cnode.h"

using namespace std;
using namespace Ogre;

class OgreManager : public WindowEventListener, 
                    public FrameListener 
{

    public:
        OgreManager();
        ~OgreManager();

        int start(ei_x_buff* resp);
        void stop();

        int renderStart();
        void renderStop();
   
        bool frameStarted(const FrameEvent &event);
        void windowClosed(RenderWindow* rw);

    protected:
        void setupInputDevices();
        void setupResources();

        Root* root;
        RenderWindow* window;
        bool running;

        OIS::InputManager* input;
        OIS::Mouse*        mouse;
        OIS::Keyboard*     keyboard;
};

OgreManager::OgreManager(){
    root = NULL;
    window = NULL;
    input = NULL;
    mouse = NULL;
    keyboard = NULL;

    running = false;
}

OgreManager::~OgreManager(){

}

int OgreManager::start(ei_x_buff* resp){
    int rc = 0;

    if( running ){
        ei_x_format(resp, "{error,already_running}");\
        goto start_exit;
    }

    if( !root ){
        root = new Root("/etc/OGRE/plugins.cfg");
    }

    if( !root->restoreConfig() && !root->showConfigDialog() ){
        ei_x_format(resp, "{error,not_configured}");
        goto start_exit;
    }

    window = root->initialise( true, "OGRE" );

    printf("After init!\n");

    //Setup OIS
    setupInputDevices();

    printf("INPUT SETUP!\n");

    //Setup Ogre Resources
    setupResources();

    root->addFrameListener(this);
    WindowEventUtilities::addWindowEventListener(window, this);

    printf("Framelistener added!\n");

    printf("PROPER!!\n");
    renderStart();

    ei_x_format(resp, "ok");

    printf("Returning OK!\n");

    start_exit:
    return rc;
}

void OgreManager::stop(){
    
    delete root;

    root = NULL;
    window = NULL;
    input = NULL;
    mouse = NULL;
    keyboard = NULL;

    running = false;
}

void OgreManager::setupInputDevices(){
    OIS::ParamList pl;
    size_t windowHnd = 0;
    ostringstream windowHndStr;

    window->getCustomAttribute("WINDOW", &windowHnd);

    windowHndStr << windowHnd;

    pl.insert( make_pair(string("WINDOW"), windowHndStr.str()) );

    input = OIS::InputManager::createInputSystem(pl);

    keyboard = static_cast<OIS::Keyboard*>(input->createInputObject(OIS::OISKeyboard, false));

    if( input->getNumberOfDevices(OIS::OISMouse) > 0 ){
        mouse = static_cast<OIS::Mouse*>(input->createInputObject(OIS::OISMouse, false));
    }

    unsigned int width, height, depth;
    int left, top;
    window->getMetrics(width, height, depth, left, top);

    //Tell OIS about the size of our window
    const OIS::MouseState &ms = mouse->getMouseState();
    ms.width = width;
    ms.height = height;
} 

void OgreManager::setupResources() {


}

bool OgreManager::frameStarted(const FrameEvent &evt){
   
    keyboard->capture();

    if( mouse ){
        mouse->capture();
    }

    if( keyboard->isKeyDown(OIS::KC_ESCAPE) ){
        this->renderStop();
        return false;
    }

    return true;
}

void OgreManager::windowClosed(RenderWindow* rw){
    this->renderStop();
}

int OgreManager::renderStart(){
    
    if( root ){
        root->startRendering();
    }

    return 0;
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

    if( input ){

        if( mouse ){
            input->destroyInputObject(mouse);
            mouse = NULL;
        }

        if( keyboard ){
            input->destroyInputObject(keyboard);
            keyboard = NULL;
        }

        OIS::InputManager::destroyInputSystem(input);
        
        input = NULL;
    }

    if( root ){
        root->removeFrameListener(this);
        WindowEventUtilities::removeWindowEventListener(window, this);

        root->queueEndRendering();
    }
}

extern "C" {

GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) new OgreManager();
}

GEN_CNODE_DEFINE(start){
    return ((OgreManager*)state)->start(resp);
}

GEN_CNODE_DEFINE(stop){
    ((OgreManager*)state)->stop();
    return 0;
}

GEN_CNODE_DEFINE(render_start){
    return ((OgreManager*)state)->renderStart();
}

GEN_CNODE_DEFINE(render_stop){
    ((OgreManager*)state)->renderStop();
    return 0;
}

}
