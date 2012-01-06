#ifndef OGRELISTENER_H
#define OGRELISTENER_H

#include<OgreRoot.h>
#include<OgreRenderWindow.h>
#include<OgreFrameListener.h>
#include<OgreWindowEventUtitilies.h>

namespace Ymir {

    class OgreListener : public Ogre::FrameListener,
                         public Ogre::WindowEventListener {

        public:
            OgreListener();
            ~OgreListener();

        protected:

            //Ogre Event Hooks
            virtual bool frameStarted( const Ogre::FrameEvent &e ) = 0;
            virtual bool frameEnded( const Ogre::FrameEvent &e ) = 0;
            virtual void windowMoved( Ogre::RenderWindow *window ) = 0;
            virtual void windowResized( Ogre::RenderWindow *window ) = 0;
            virtual bool windowClosing( Ogre::RenderWindow *window ) = 0;
            virtual void windowClosed( Ogre::RenderWindow *window ) = 0;
            virtual void windowFocusChanged( Ogre::RenderWindow *window ) = 0;
    };

    class OISListener : public OgreListener {

        public:
            OISOgreListener() : OgreListener(){}
            ~OISOgreListener(){}

        protected:

            //Ogre Event Hooks
            bool frameStarted( const Ogre::FrameEvent &e );
            bool frameEnded( const Ogre::FrameEvent &e );
            void windowMoved( Ogre::RenderWindow *window );
            void windowResized( Ogre::RenderWindow *window );
            bool windowClosing( Ogre::RenderWindow *window );
            void windowClosed( Ogre::RenderWindow *window );
            void windowFocusChanged( Ogre::RenderWindow *window );
    };
}
#endif
