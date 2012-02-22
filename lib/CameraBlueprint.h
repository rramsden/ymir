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
            static void setCameraPosition(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraMove(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraDirection(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraLookAt(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraYaw(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraPitch(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setCameraRoll(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setNearClip(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setFarClip(NodeInfo<Ogre::Camera>*, boost::any&);
            static void setFixYaw(NodeInfo<Ogre::Camera>*, boost::any&);

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
