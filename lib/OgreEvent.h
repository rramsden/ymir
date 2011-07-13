#ifndef OGREEVENT_H
#define OGREEVENT_H

#include <OIS.h>

#include <gen_cnode.h>

class OgreEvent : public OIS::KeyListener,
                  public OIS::MouseListener {

    int encodeMouseEvent( OIS::MouseButtonID id,
                          const OIS::MouseEvent& event, 
                          ei_x_buff* output );
    public:
        bool keyPressed( const OIS::KeyEvent& e );
        bool keyReleased( const OIS::KeyEvent& e );

        bool mouseMoved( const OIS::MouseEvent &e );
        bool mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id );
        bool mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id );
};

#endif
