#include "EventManager.h"
#include "Core.h"

using namespace Ymir;

namespace Ymir {
    EventManager *EventManager::mEventManager;
     
    EventManager::EventManager( void ) :
        rendering( true ),
        gui(NULL),
        mMouse( 0 ),
        mKeyboard( 0 ),
        mInputSystem( 0 ),
        mLock(),
        mTasks(),
        mEventListeners(),
        itTask(),
        itEventListener(),
        itTaskEnd(),
        itEventListenerEnd() {
    }
     
    EventManager::~EventManager( void ) {
        if( mInputSystem ) {
            if( mMouse ) {
                mInputSystem->destroyInputObject( mMouse );
                mMouse = 0;
            }
     
            if( mKeyboard ) {
                mInputSystem->destroyInputObject( mKeyboard );
                mKeyboard = 0;
            }
     
            /*if( mJoysticks.size() > 0 ) {
                itJoystick    = mJoysticks.begin();
                itJoystickEnd = mJoysticks.end();
                for(; itJoystick != itJoystickEnd; ++itJoystick ) {
                    mInputSystem->destroyInputObject( *itJoystick );
                }
     
                mJoysticks.clear();
            }*/
     
            // If you use OIS1.0RC1 or above, uncomment this line
            // and comment the line below it
            mInputSystem->destroyInputSystem( mInputSystem );
            //mInputSystem->destroyInputSystem();
            mInputSystem = 0;
     
            mEventListeners.clear();
            // Clear Listeners
            /*mKeyListeners.clear();
            mMouseListeners.clear();
            mJoystickListeners.clear();
            mFrameListeners.clear();*/
        }
    }
     
    void EventManager::initialise( Ogre::RenderWindow *renderWindow ) {
        if( !mInputSystem ) {
            // Setup basic variables
            OIS::ParamList paramList;    
            size_t windowHnd = 0;
            std::ostringstream windowHndStr;
     
            // Get window handle
            renderWindow->getCustomAttribute( "WINDOW", &windowHnd );
     
            // Fill parameter list
            windowHndStr << (unsigned int) windowHnd;
            paramList.insert( std::make_pair( std::string( "WINDOW" ), windowHndStr.str() ) );
     
            // Create inputsystem
            mInputSystem = OIS::InputManager::createInputSystem( paramList );
     
            // If possible create a buffered keyboard
            // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISKeyboard) > 0) {
            //if( mInputSystem->numKeyboards() > 0 ) {
            if (mInputSystem->getNumberOfDevices(OIS::OISKeyboard) > 0) {
                mKeyboard = static_cast<OIS::Keyboard*>( mInputSystem->createInputObject( OIS::OISKeyboard, true ) );
                mKeyboard->setEventCallback( this );
            }
     
            // If possible create a buffered mouse
            // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISMouse) > 0) {
            //if( mInputSystem->numMice() > 0 ) {
            if (mInputSystem->getNumberOfDevices(OIS::OISMouse) > 0) {
                mMouse = static_cast<OIS::Mouse*>( mInputSystem->createInputObject( OIS::OISMouse, true ) );
                mMouse->setEventCallback( this );
     
                // Get window size
                unsigned int width, height, depth;
                int left, top;
                renderWindow->getMetrics( width, height, depth, left, top );
     
                // Set mouse region
                this->setWindowExtents( width, height );
            }
    
            Ogre::WindowEventUtilities::addWindowEventListener(renderWindow, this);
    
            // If possible create all joysticks in buffered mode
            // (note: if below line doesn't compile, try:  if (mInputSystem->getNumberOfDevices(OIS::OISJoyStick) > 0) {
            //if( mInputSystem->numJoySticks() > 0 ) {
            /*if (mInputSystem->getNumberOfDevices(OIS::OISJoyStick) > 0) {
                //mJoysticks.resize( mInputSystem->numJoySticks() );
                mJoysticks.resize( mInputSystem->getNumberOfDevices(OIS::OISJoyStick) );
     
                itJoystick    = mJoysticks.begin();
                itJoystickEnd = mJoysticks.end();
                for(; itJoystick != itJoystickEnd; ++itJoystick ) {
                    (*itJoystick) = static_cast<OIS::JoyStick*>( mInputSystem->createInputObject( OIS::OISJoyStick, true ) );
                    (*itJoystick)->setEventCallback( this );
                }
            }*/
        }
    }
     
    void EventManager::capture( void ) {
        // Need to capture / update each device every frame
        if( mMouse ) {
            mMouse->capture();
        }
     
        if( mKeyboard ) {
            mKeyboard->capture();
        }
     
        /*if( mJoysticks.size() > 0 ) {
            itJoystick    = mJoysticks.begin();
            itJoystickEnd = mJoysticks.end();
            for(; itJoystick != itJoystickEnd; ++itJoystick ) {
                (*itJoystick)->capture();
            }
        }*/
    }
    
    void EventManager::setRendering( bool rendering ){
        this->rendering = rendering;
    }
    
    void EventManager::queueTask( Ymir::Task* task ){
   
        mLock.lock();
        mTasks.push_back(task);
        mLock.unlock();
    }
    
    void EventManager::addEventListener( EventListener *eventListener, const std::string& instanceName ){
    
        itEventListener = mEventListeners.find( instanceName );
        if( itEventListener == mEventListeners.end() ){
                mEventListeners[ instanceName ] = eventListener;
            }
            else {
                // Duplicate Item
            }
    }
    
    /*void EventManager::addKeyListener( OIS::KeyListener *keyListener, const std::string& instanceName ) {
        if( mKeyboard ) {
            // Check for duplicate items
            itKeyListener = mKeyListeners.find( instanceName );
            if( itKeyListener == mKeyListeners.end() ) {
                mKeyListeners[ instanceName ] = keyListener;
            }
            else {
                // Duplicate Item
            }
        }
    }
     
    void EventManager::addMouseListener( OIS::MouseListener *mouseListener, const std::string& instanceName ) {
        if( mMouse ) {
            // Check for duplicate items
            itMouseListener = mMouseListeners.find( instanceName );
            if( itMouseListener == mMouseListeners.end() ) {
                mMouseListeners[ instanceName ] = mouseListener;
            }
            else {
                // Duplicate Item
            }
        }
    }
     
    void EventManager::addJoystickListener( OIS::JoyStickListener *joystickListener, const std::string& instanceName ) {
        if( mJoysticks.size() > 0 ) {
            // Check for duplicate items
            itJoystickListener = mJoystickListeners.find( instanceName );
            if( itJoystickListener == mJoystickListeners.end() ) {
                mJoystickListeners[ instanceName ] = joystickListener;
            }
            else {
                // Duplicate Item
            }
        }
    }
    
    void EventManager::addFrameListener( Ogre::FrameListener *frameListener, const std::string& instanceName ){
        if( mFrameListeners.size() > 0 ) {
            // Check for duplicate items
            itFrameListener = mFrameListeners.find( instanceName );
            if( itFrameListener == mFrameListeners.end() ) {
                mFrameListeners[ instanceName ] = frameListener;
            }
            else {
                // Duplicate Item
            }
        }
    }*/
    
    void EventManager::removeEventListener( const std::string& instanceName ) {
        // Check if item exists
        itEventListener = mEventListeners.find( instanceName );
        if( itEventListener != mEventListeners.end() ) {
            mEventListeners.erase( itEventListener );
        }
        else {
            // Doesn't Exist
        }
    }
    
    /*void EventManager::removeKeyListener( const std::string& instanceName ) {
        // Check if item exists
        itKeyListener = mKeyListeners.find( instanceName );
        if( itKeyListener != mKeyListeners.end() ) {
            mKeyListeners.erase( itKeyListener );
        }
        else {
            // Doesn't Exist
        }
    }
     
    void EventManager::removeMouseListener( const std::string& instanceName ) {
        // Check if item exists
        itMouseListener = mMouseListeners.find( instanceName );
        if( itMouseListener != mMouseListeners.end() ) {
            mMouseListeners.erase( itMouseListener );
        }
        else {
            // Doesn't Exist
        }
    }
     
    void EventManager::removeJoystickListener( const std::string& instanceName ) {
        // Check if item exists
        itJoystickListener = mJoystickListeners.find( instanceName );
        if( itJoystickListener != mJoystickListeners.end() ) {
            mJoystickListeners.erase( itJoystickListener );
        }
        else {
            // Doesn't Exist
        }
    }
    
    void EventManager::removeFrameListener( const std::string& instanceName ) {
        // Check if item exists
        itFrameListener = mFrameListeners.find( instanceName );
        if( itFrameListener != mFrameListeners.end() ) {
            mFrameListeners.erase( itFrameListener );
        }
        else {
            // Doesn't Exist
        }
    }*/
    
    void EventManager::removeEventListener( EventListener *eventListener ) {
        itEventListener    = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if( itEventListener->second == eventListener ) {
                mEventListeners.erase( itEventListener );
                break;
            }
        }
    }
    
    /*void EventManager::removeKeyListener( OIS::KeyListener *keyListener ) {
        itKeyListener    = mKeyListeners.begin();
        itKeyListenerEnd = mKeyListeners.end();
        for(; itKeyListener != itKeyListenerEnd; ++itKeyListener ) {
            if( itKeyListener->second == keyListener ) {
                mKeyListeners.erase( itKeyListener );
                break;
            }
        }
    }
     
    void EventManager::removeMouseListener( OIS::MouseListener *mouseListener ) {
        itMouseListener    = mMouseListeners.begin();
        itMouseListenerEnd = mMouseListeners.end();
        for(; itMouseListener != itMouseListenerEnd; ++itMouseListener ) {
            if( itMouseListener->second == mouseListener ) {
                mMouseListeners.erase( itMouseListener );
                break;
            }
        }
    }
     
    void EventManager::removeJoystickListener( OIS::JoyStickListener *joystickListener ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if( itJoystickListener->second == joystickListener ) {
                mJoystickListeners.erase( itJoystickListener );
                break;
            }
        }
    }
    
    void EventManager::removeFrameListener( Ogre::FrameListener *frameListener ) {
        itFrameListener    = mFrameListeners.begin();
        itFrameListenerEnd = mFrameListeners.end();
        for(; itFrameListener != itFrameListenerEnd; ++itFrameListener ) {
            if( itFrameListener->second == frameListener ) {
                mFrameListeners.erase( itFrameListener );
                break;
            }
        }
    }*/
    
    void EventManager::removeAllListeners( void ) {
        mEventListeners.clear();
        /*mKeyListeners.clear();
        mMouseListeners.clear();
        mJoystickListeners.clear();
        mFrameListeners.clear();*/
    }
     
    /*void EventManager::removeAllKeyListeners( void ) {
        mKeyListeners.clear();
    }
     
    void EventManager::removeAllMouseListeners( void ) {
        mMouseListeners.clear();
    }
     
    void EventManager::removeAllJoystickListeners( void ) {
        mJoystickListeners.clear();
    }
    
    void EventManager::removeAllFrameListeners( void ) {
        mFrameListeners.clear();
    }*/
    
    void EventManager::setWindowExtents( int width, int height ) {
        // Set mouse region (if window resizes, we should alter this to reflect as well)
        const OIS::MouseState &mouseState = mMouse->getMouseState();
        mouseState.width  = width;
        mouseState.height = height;
    }
    
    OIS::Mouse* EventManager::getMouse( void ) {
        return mMouse;
    }
     
    OIS::Keyboard* EventManager::getKeyboard( void ) {
        return mKeyboard;
    }
     
    /*OIS::JoyStick* EventManager::getJoystick( unsigned int index ) {
        // Make sure it's a valid index
        if( index < mJoysticks.size() ) {
            return mJoysticks[ index ];
        }
     
        return 0;
    }
     
    int EventManager::getNumOfJoysticks( void ) {
        // Cast to keep compiler happy ^^
        return (int) mJoysticks.size();
    }*/
    bool EventManager::frameStarted( const Ogre::FrameEvent &e ){
     
        this->capture();
    
        //Perform the tasks in our queue
        mLock.lock();
        itTask = mTasks.begin();
        itTaskEnd = mTasks.end();
        for(; itTask != itTaskEnd; ++itTask){
           (*itTask)->run();
        }
   
        mTasks.clear();
        mLock.unlock();

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
    
    bool EventManager::frameEnded( const Ogre::FrameEvent &e ){
     
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
    
    void EventManager::monitor(MyGUI::Widget* widget){
    
        widget->eventMouseButtonPressed = MyGUI::newDelegate(this, &EventManager::guiMousePressed);
        widget->eventMouseButtonReleased = MyGUI::newDelegate(this, &EventManager::guiMouseReleased);
        widget->eventMouseButtonClick = MyGUI::newDelegate(this, &EventManager::guiMouseButtonClick);
        /*widget->eventMouseButtonDoubleClick += (em, &EventManager::mouseDoubleClicked);
    
        //Sense all support events on this object
        widget->eventMouseLostFocus += (em, &EventManager::mouseLostFocus);
        widget->eventMouseSetFocus += (em, &EventManager::mouseSetFocus);
        widget->eventMouseMove += (em, &EventManager::mouseMove);
        widget->eventMouseWheel += (em, &EventManager::mouseWheel);
    
        widget->eventKeyButtonPressed += (em, &EventManager::keyPressed);
        widget->eventKeyButtonReleased += (em, &EventManager::keyReleased);
        */
    }
    
    bool EventManager::keyPressed( const OIS::KeyEvent &e ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        if( MyGUI::InputManager::getInstancePtr() ){
            MyGUI::InputManager::getInstancePtr()->injectKeyPress( MyGUI::KeyCode::Enum(e.key), e.text );
        }
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->keyPressed( e ))
                break;
        }
     
        return true;
    }
    
    bool EventManager::keyReleased( const OIS::KeyEvent &e ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        if( MyGUI::InputManager::getInstancePtr() ){
            MyGUI::InputManager::getInstancePtr()->injectKeyRelease(MyGUI::KeyCode::Enum(e.key));
        }
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->keyReleased( e ))
                break;
        }
     
        return true;
    }
    
    bool EventManager::mouseMoved( const OIS::MouseEvent &e ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        if( MyGUI::InputManager::getInstancePtr() ){
            MyGUI::InputManager::getInstancePtr()->injectMouseMove(e.state.X.abs, e.state.Y.abs, e.state.Z.abs);
        }
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->mouseMoved( e ))
                break;
        }
     
        return true;
    }
    
    bool EventManager::mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        if( MyGUI::InputManager::getInstancePtr() ){
            MyGUI::InputManager::getInstancePtr()->injectMousePress( e.state.X.abs, 
                                   e.state.Y.abs, 
                                   MyGUI::MouseButton::Enum(id) ); 
        }
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->mousePressed( e, id ))
                break;
        }
     
        return true;
    }
    
    bool EventManager::mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        if( MyGUI::InputManager::getInstancePtr() ){
            MyGUI::InputManager::getInstancePtr()->injectMouseRelease( e.state.X.abs, 
                                                              e.state.Y.abs, 
                                                              MyGUI::MouseButton::Enum(id) );
        }
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->mouseReleased( e, id ))
                break;
        }
     
        return true;
    }
    
    void EventManager::guiMousePressed(MyGUI::Widget* widget,
                                       int left, int right,
                                       MyGUI::MouseButton id)
    {
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
   
        Ymir::Core::getSingletonPtr()->logNormal("Inside guiMousePressed!!");

        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->guiMousePressed(widget, left, right, id);
        }
    }
    
    void EventManager::guiMouseReleased(MyGUI::Widget* widget,
                                        int left, int right,
                                        MyGUI::MouseButton id)
    {
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
   
        Ymir::Core::getSingletonPtr()->logNormal("Inside guiMouseReleased!!");

        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->guiMouseReleased(widget, left, right, id);
        }
    }
    
    void EventManager::guiMouseButtonClick(MyGUI::Widget* widget)
    {
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
   
        Ymir::Core::getSingletonPtr()->logNormal("Inside guiMouseButtonClick!!");

        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->guiMouseButtonClick(widget);
        }
    }
    /*bool EventManager::keyPressed( const OIS::KeyEvent &e ) {
        itKeyListener    = mKeyListeners.begin();
        itKeyListenerEnd = mKeyListeners.end();
        for(; itKeyListener != itKeyListenerEnd; ++itKeyListener ) {
            if(!itKeyListener->second->keyPressed( e ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::keyReleased( const OIS::KeyEvent &e ) {
        itKeyListener    = mKeyListeners.begin();
        itKeyListenerEnd = mKeyListeners.end();
        for(; itKeyListener != itKeyListenerEnd; ++itKeyListener ) {
            if(!itKeyListener->second->keyReleased( e ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::mouseMoved( const OIS::MouseEvent &e ) {
        itMouseListener    = mMouseListeners.begin();
        itMouseListenerEnd = mMouseListeners.end();
        for(; itMouseListener != itMouseListenerEnd; ++itMouseListener ) {
            if(!itMouseListener->second->mouseMoved( e ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::mousePressed( const OIS::MouseEvent &e, OIS::MouseButtonID id ) {
        itMouseListener    = mMouseListeners.begin();
        itMouseListenerEnd = mMouseListeners.end();
        for(; itMouseListener != itMouseListenerEnd; ++itMouseListener ) {
            if(!itMouseListener->second->mousePressed( e, id ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::mouseReleased( const OIS::MouseEvent &e, OIS::MouseButtonID id ) {
        itMouseListener    = mMouseListeners.begin();
        itMouseListenerEnd = mMouseListeners.end();
        for(; itMouseListener != itMouseListenerEnd; ++itMouseListener ) {
            if(!itMouseListener->second->mouseReleased( e, id ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::povMoved( const OIS::JoyStickEvent &e, int pov ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if(!itJoystickListener->second->povMoved( e, pov ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::axisMoved( const OIS::JoyStickEvent &e, int axis ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if(!itJoystickListener->second->axisMoved( e, axis ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::sliderMoved( const OIS::JoyStickEvent &e, int sliderID ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if(!itJoystickListener->second->sliderMoved( e, sliderID ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::buttonPressed( const OIS::JoyStickEvent &e, int button ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if(!itJoystickListener->second->buttonPressed( e, button ))
                break;
        }
     
        return true;
    }
     
    bool EventManager::buttonReleased( const OIS::JoyStickEvent &e, int button ) {
        itJoystickListener    = mJoystickListeners.begin();
        itJoystickListenerEnd = mJoystickListeners.end();
        for(; itJoystickListener != itJoystickListenerEnd; ++itJoystickListener ) {
            if(!itJoystickListener->second->buttonReleased( e, button ))
                break;
        }
     
        return true;
    }*/
    
    void EventManager::windowMoved( Ogre::RenderWindow *window ) {
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowMoved(window);
        }
    }
    
    void EventManager::windowResized( Ogre::RenderWindow *window ){
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
    
    bool EventManager::windowClosing( Ogre::RenderWindow *window ){ 
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            if(!itEventListener->second->windowClosing(window)){
                return false;
            }
        }
    
        return true;
    }
    
    void EventManager::windowClosed( Ogre::RenderWindow *window ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowClosed(window);
        }
    
        rendering = false;
    }
    
    void EventManager::windowFocusChanged( Ogre::RenderWindow *window ){
        itEventListener = mEventListeners.begin();
        itEventListenerEnd = mEventListeners.end();
    
        for(; itEventListener != itEventListenerEnd; ++itEventListener ) {
            itEventListener->second->windowFocusChanged(window);
        }
    }
    
    EventManager* EventManager::getSingletonPtr( void ) {
        if( !mEventManager ) {
            mEventManager = new EventManager();
        }
     
        return mEventManager;
    }
}
