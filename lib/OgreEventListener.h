#ifndef OGREEVENTLISTENER_H
#define OGREEVENTLISTENER_H

#include <gen_cnode.h>

#include "EventListener.h"

class OgreEventListener : public EventListener {

    int encodeMouseEvent( OIS::MouseButtonID id,
                          const OIS::MouseEvent& event, 
                          ei_x_buff* output );

    public:
        bool keyPressed( const OIS::KeyEvent& e );
        bool keyReleased( const OIS::KeyEvent& e );

        bool mouseMoved( const OIS::MouseEvent &e );
        bool mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id );
        bool mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id );

        bool frameStarted( const Ogre::FrameEvent& e );
        bool frameEnded( const Ogre::FrameEvent& e );

        void guiMousePressed(MyGUI::Widget* widget,
                             int left, int right,
                             MyGUI::MouseButton id);

        void guiMouseReleased(MyGUI::Widget* widget,
                              int left, int right,
                              MyGUI::MouseButton id);

        void guiMouseButtonClick(MyGUI::Widget* widget);

        void windowClosed( Ogre::RenderWindow* window );
};

#endif
