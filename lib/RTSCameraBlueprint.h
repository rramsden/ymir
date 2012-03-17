#ifndef _RTSCAMERABLUEPRINT_H
#define _RTSCAMERABLUEPRINT_H

#include <CCSBasicCameraModes.h>

#include "CameraModeBlueprint.h"

namespace Ymir {

    class RTSCameraBlueprint : public CameraModeBlueprint {

        public:
            RTSCameraBlueprint() : CameraModeBlueprint() {
                mBlueprint["move"] = BPFP(NULL, (setFP)&setMove);
                mBlueprint["moveSpeed"] = BPFP(NULL, (setFP)&setMoveSpeed);
                mBlueprint["zoom"] = BPFP(NULL, (setFP)&setZoom);
            }

            ~RTSCameraBlueprint(){}

            static void setMove(CCS::RTSCameraMode* mode, boost::any& val){
                Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);
               
                vec.z >= 0.0f ? mode->zoomIn(vec.z) : mode->zoomOut(vec.z);
                vec.y >= 0.0f ? mode->goUp(vec.y) : mode->goDown(vec.y);
                vec.x >= 0.0f ? mode->goRight(vec.x) : mode->goLeft(vec.x);  
            }

            static void setMoveSpeed(CCS::RTSCameraMode* mode, boost::any& val){
                Ogre::Real sp = boost::any_cast<Ogre::Real>(val);

                mode->setMoveFactor(sp);
            }

            static void setZoom(CCS::RTSCameraMode* mode, boost::any& val){
                Ogre::Real zoom = boost::any_cast<Ogre::Real>(val);

                mode->setZoom(zoom);
            }

        protected:

            CCS::RTSCameraMode* createMode( std::string& id,
                    PropList& props,
                    CCS::CameraControlSystem* ccs )
            {
                Ogre::Real minZoom = 0, maxZoom=0;
               
                props.hasProperty("minZoom", &minZoom);
                props.hasProperty("maxZoom", &maxZoom); 

                return new CCS::RTSCameraMode(ccs,Ogre::Vector3::ZERO, 
                        Ogre::Vector3::NEGATIVE_UNIT_Z, Ogre::Vector3::NEGATIVE_UNIT_X, 
                        Ogre::Radian(Ogre::Degree(90)), 
                        minZoom, maxZoom); 
            }
    };
}
#endif
