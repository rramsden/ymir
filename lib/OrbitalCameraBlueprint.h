#ifndef _ORBITALCAMERABLUEPRINT_H
#define _ORBITALCAMERABLUEPRINT_H

#include <CCSOrbitalCameraMode.h>

#include "CameraModeBlueprint.h"

namespace Ymir {

    class OrbitalCameraBlueprint : public CameraModeBlueprint {

        public:
            OrbitalCameraBlueprint() : CameraModeBlueprint() {
                mBlueprint["rotate"] = BPFP(NULL, (setFP)&setRotate);
                mBlueprint["rotateSpeed"] = BPFP(NULL, (setFP)&setRotateSpeed);
                mBlueprint["zoom"] = BPFP(NULL, (setFP)&setZoom);
                mBlueprint["zoomSpeed"] = BPFP(NULL, (setFP)&setZoomSpeed);
            }

            ~OrbitalCameraBlueprint(){}

            static void setRotate(CCS::OrbitalCameraMode* mode, boost::any& val){
               Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);

               mode->pitch(Ogre::Degree(vec.y).valueRadians());
               mode->yaw(Ogre::Degree(vec.x).valueRadians());
            }

            static void setRotateSpeed(CCS::OrbitalCameraMode* mode, boost::any& val){
                Ogre::Real sp = boost::any_cast<Ogre::Real>(val);

                mode->setRotationFactor(Ogre::Degree(sp).valueRadians());
            }
            
            static void setZoom(CCS::OrbitalCameraMode* mode, boost::any& val){
                Ogre::Real zoom = boost::any_cast<Ogre::Real>(val);

                mode->setZoom(zoom);
            }

            static void setZoomSpeed(CCS::OrbitalCameraMode* mode, boost::any& val){
                Ogre::Real sp = boost::any_cast<Ogre::Real>(val);

                mode->setZoomFactor(sp);
            }


        protected:
        
            CCS::OrbitalCameraMode* createMode( std::string& id,
                    PropList& props, 
                    CCS::CameraControlSystem* ccs )
            {
                return new CCS::OrbitalCameraMode(ccs);
            }
    };
}

#endif
