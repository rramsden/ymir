#ifndef EVENTLISTENER_H
#define EVENTLISTENER_H

#include <OIS.h>
#include <OgreFrameListener.h>
#include <OgreWindowEventUtilities.h>

class EventListener : public OIS::KeyListener,
                      public OIS::MouseListener,
                      public Ogre::FrameListener,
                      public Ogre::WindowEventListener {


    public:
        virtual bool keyPressed( const OIS::KeyEvent& e ) { return true; }
        virtual bool keyReleased( const OIS::KeyEvent& e ) { return true; }

        virtual bool mouseMoved( const OIS::MouseEvent &e ) { return true; }
        virtual bool mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id ) { return true; }
        virtual bool mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id ) {return true; }

        virtual bool frameStarted( const Ogre::FrameEvent& e ) { return true; }
        virtual bool frameEnded( const Ogre::FrameEvent& e ) { return true; }
        virtual bool frameRenderingQueued( const Ogre::FrameEvent &e ) { return true; } 

        virtual void windowMoved( Ogre::RenderWindow *window ) {}
        virtual void windowResized( Ogre::RenderWindow *window ){}
        virtual bool windowClosing( Ogre::RenderWindow *window ){ return true; }
        virtual void windowClosed( Ogre::RenderWindow *window ){}
        virtual void windowFocusChanged( Ogre::RenderWindow *window ){}
};

#endif
