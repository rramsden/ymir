#ifndef EventManager_H
#define EventManager_H
 
#include <OISInputManager.h>
#include <MyGUI.h>
#include <OgreRenderWindow.h>

#include "Task.h"
#include "EventListener.h"

namespace Ymir {
    class EventManager : public EventListener {
    
    public:
        virtual ~EventManager( void );
     
        void initialise( Ogre::RenderWindow *renderWindow );
        void capture( void );
        void setRendering( bool rendering );
    
        bool frameStarted( const Ogre::FrameEvent &e );
        bool frameEnded( const Ogre::FrameEvent &e );
    
        void monitor(MyGUI::Widget* widget);
    
        void queueTask( Ymir::Task* task );
    
        void addEventListener( EventListener* eventListener, const std::string& instanceName );
        /*void addKeyListener( OIS::KeyListener *keyListener, const std::string& instanceName );
        void addMouseListener( OIS::MouseListener *mouseListener, const std::string& instanceName );
        void addJoystickListener( OIS::JoyStickListener *joystickListener, const std::string& instanceName );
        void addFrameListener( Ogre::FrameListener *frameListener, const std::string& instanceName );*/
    
        void removeEventListener( const std::string& instanceName );
        /*void removeKeyListener( const std::string& instanceName );
        void removeMouseListener( const std::string& instanceName );
        void removeJoystickListener( const std::string& instanceName );
        void removeFrameListener( const std::string& instanceName );*/
    
        void removeEventListener( EventListener* eventListener );
        /*void removeKeyListener( OIS::KeyListener *keyListener );
        void removeMouseListener( OIS::MouseListener *mouseListener );
        void removeJoystickListener( OIS::JoyStickListener *joystickListener );
        void removeFrameListener( Ogre::FrameListener *frameListener );*/
    
        void removeAllListeners( void );
        /*void removeAllKeyListeners( void );
        void removeAllMouseListeners( void );
        void removeAllJoystickListeners( void );
        void removeAllFrameListeners( void );*/
    
        void setWindowExtents( int width, int height );
    
        OIS::Mouse*    getMouse( void );
        OIS::Keyboard* getKeyboard( void );
        /*OIS::JoyStick* getJoystick( unsigned int index );
     
        int getNumOfJoysticks( void );*/
     
        static EventManager* getSingletonPtr( void );
    private:
        EventManager( void );
        EventManager( const EventManager& ) { }
        EventManager & operator = ( const EventManager& );
    
        bool keyPressed( const OIS::KeyEvent &e );
        bool keyReleased( const OIS::KeyEvent &e );
     
        bool mouseMoved( const OIS::MouseEvent &e );
        bool mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id );
        bool mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id );
        
        void guiMousePressed(MyGUI::Widget* widget,
                             int left, int right,
                             MyGUI::MouseButton id);
    
        void guiMouseReleased(MyGUI::Widget* widget,
                              int left, int right,
                              MyGUI::MouseButton id);
   
        void guiMouseButtonClick(MyGUI::Widget* widget);

        void windowMoved( Ogre::RenderWindow *window );
        void windowResized( Ogre::RenderWindow *window );
        bool windowClosing( Ogre::RenderWindow *window );
        void windowClosed( Ogre::RenderWindow *window );
        void windowFocusChanged( Ogre::RenderWindow *window );
    
        /*bool povMoved( const OIS::JoyStickEvent &e, int pov );
        bool axisMoved( const OIS::JoyStickEvent &e, int axis );
        bool sliderMoved( const OIS::JoyStickEvent &e, int sliderID );
        bool buttonPressed( const OIS::JoyStickEvent &e, int button );
        bool buttonReleased( const OIS::JoyStickEvent &e, int button );*/
        bool rendering;
    
        MyGUI::InputManager* gui;
    
        OIS::Mouse        *mMouse;
        OIS::Keyboard     *mKeyboard;
        OIS::InputManager *mInputSystem;
    
        std::vector<Ymir::Task*> mTasks; 
        /*std::vector<OIS::JoyStick*> mJoysticks;
        std::vector<OIS::JoyStick*>::iterator itJoystick;
        std::vector<OIS::JoyStick*>::iterator itJoystickEnd;*/
    
        std::map<std::string, EventListener*> mEventListeners;
        /*std::map<std::string, OIS::KeyListener*> mKeyListeners;
        std::map<std::string, OIS::MouseListener*> mMouseListeners;
        std::map<std::string, OIS::JoyStickListener*> mJoystickListeners;
        std::map<std::string, Ogre::FrameListener*> mFrameListeners;*/
    
        std::vector<Ymir::Task*>::iterator itTask;
        std::map<std::string, EventListener*>::iterator itEventListener;
        /*std::map<std::string, OIS::KeyListener*>::iterator itKeyListener;
        std::map<std::string, OIS::MouseListener*>::iterator itMouseListener;
        std::map<std::string, OIS::JoyStickListener*>::iterator itJoystickListener;
        std::map<std::string, Ogre::FrameListener*>::iterator itFrameListener;*/
    
        std::vector<Ymir::Task*>::iterator itTaskEnd;
        std::map<std::string, EventListener*>::iterator itEventListenerEnd;
        /*std::map<std::string, OIS::KeyListener*>::iterator itKeyListenerEnd;
        std::map<std::string, OIS::MouseListener*>::iterator itMouseListenerEnd;
        std::map<std::string, OIS::JoyStickListener*>::iterator itJoystickListenerEnd;
        std::map<std::string, Ogre::FrameListener*>::iterator itFrameListenerEnd;*/
    
        static EventManager *mEventManager;
    
    };
}
#endif
