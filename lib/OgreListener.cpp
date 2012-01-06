#include "OgreListener.h"
#include "InputListener.h"

using namespace Ymir;

OgreListener::OgreListener() {

}

OgreListener::~OgreListener(){

}

/*void OgreListener::initialise( Ogre::RenderWindow* window ){
    Ogre::Root::getSingletonPtr()->addFrameListener(em);
    Ogre::WindowEventUtilities::addWindowEventListener(window, this);
}*/


bool OISListener::frameStarted( const Ogre::FrameEvent &e ){
     
        this->capture();
    
        /*//Perform some amount from our tasks list
        itTask = mTasks.begin();
        itTaskEnd = mTasks.end();
        for(; itTask != itTaskEnd; ++itTask){
           (*itTask)->run();
        }*/
    
        itEventListener    = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            
            if(!itEventListener->second->frameStarted( e ))
            {
                rendering = false;
            }
        }
     
        return rendering;
    }
    
    bool OISListener::frameEnded( const Ogre::FrameEvent &e ){
     
        /*itEventListener    = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            
            if(!itEventListener->second->frameEnded( e ))
            {
                return false;
            }
        }*/
     
        return rendering;
    }

    void OISListener::windowMoved( Ogre::RenderWindow *window ) {
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowMoved(window);
        }
    }
    
    void OISListener::windowResized( Ogre::RenderWindow *window ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        // Get window size
        unsigned int width, height, depth;
        int left, top;
        window->getMetrics( width, height, depth, left, top );
     
        //Set new mouse region
        this->setWindowExtents( width, height );    
        
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowResized(window);
        }
    }
    
    bool OISListener::windowClosing( Ogre::RenderWindow *window ){ 
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->windowClosing(window)){
                return false;
            }
        }
    
        return true;
    }
    
    void OISListener::windowClosed( Ogre::RenderWindow *window ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowClosed(window);
        }
    
        rendering = false;
    }
    
    void OISListener::windowFocusChanged( Ogre::RenderWindow *window ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowFocusChanged(window);
        }
    }

