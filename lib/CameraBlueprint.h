#ifndef _CAMERABLUEPRINT_H
#define _CAMERABLUEPRINT_H

#include <OgreCamera.h>

#include "NodeBlueprint.h"

namespace Ymir {

    class CameraBlueprint : public NodeBlueprint<Ogre::Camera> {

        public:
            CameraBlueprint();
            ~CameraBlueprint(){}
            
            //Cameras overrides default SceneNode behavior
            static void setCameraPosition(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraMove(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraDirection(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraLookAt(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraYaw(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraPitch(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setCameraRoll(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setNearClip(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setFarClip(NodeTuple<Ogre::Camera>*, boost::any&);
            static void setFixYaw(NodeTuple<Ogre::Camera>*, boost::any&);

        protected:
            Ogre::Camera* createOgreObject( std::string&, 
                                            PropList&, 
                                            Ogre::SceneManager*);

            Ogre::Camera* findOgreObject( std::string&, 
                                          Ogre::SceneManager*);

            void destroyOgreObject( std::string&, 
                                    Ogre::SceneManager*);
    };
}
#endif
