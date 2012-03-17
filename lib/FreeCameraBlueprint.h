#ifndef _FREECAMERABLUEPRINT_H
#define _FREECAMERABLUEPRINT_H

#include <CCSFreeCameraMode.h>

#include "CameraModeBlueprint.h"

namespace Ymir {

    class FreeCameraBlueprint : public CameraModeBlueprint {

        public:
        
            FreeCameraBlueprint() : CameraModeBlueprint(){
                mBlueprint["rotate"] = BPFP(NULL, (setFP)&setRotate);
                mBlueprint["rotateSpeed"] = BPFP(NULL, (setFP)&setRotateSpeed);
                mBlueprint["position"] = BPFP(NULL, (setFP)&setPosition);
                mBlueprint["move"] = BPFP(NULL, (setFP)&setMove);
                mBlueprint["moveSpeed"] = BPFP(NULL, (setFP)&setMoveSpeed);
            }

            ~FreeCameraBlueprint(){}

            static void setRotate(CCS::FreeCameraMode* mode, boost::any& val){
               Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);

               mode->pitch(Ogre::Degree(vec.y).valueRadians());
               mode->yaw(Ogre::Degree(vec.x).valueRadians());
               //mode->roll(Ogre::Degree(vec.z).valueRadians());
            }

            static void setRotateSpeed(CCS::FreeCameraMode* mode, boost::any& val){
                Ogre::Real sp = boost::any_cast<Ogre::Real>(val);

                mode->setRotationFactor(Ogre::Degree(sp).valueRadians());
            }
            
            static void setPosition(CCS::FreeCameraMode* mode, boost::any& val){
                Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);

                mode->setCameraPosition(vec);
            }

            static void setMove(CCS::FreeCameraMode* mode, boost::any& val){
                Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);
               
                vec.z >= 0.0f ? mode->goForward(vec.z) : mode->goBackward(Math::Abs(vec.z));
                vec.y >= 0.0f ? mode->goUp(vec.y) : mode->goDown(Math::Abs(vec.y));
                vec.x >= 0.0f ? mode->goRight(vec.x) : mode->goLeft(Math::Abs(vec.x));  
            }

            static void setMoveSpeed(CCS::FreeCameraMode* mode, boost::any& val){
                Ogre::Real sp = boost::any_cast<Ogre::Real>(val);

                mode->setMoveFactor(sp);
            }

        protected:
            CCS::FreeCameraMode* createMode( std::string& id, 
                                             PropList& props,
                                             CCS::CameraControlSystem* ccs )
            {
                return new CCS::FreeCameraMode(ccs);
            }
    };
}
#endif
