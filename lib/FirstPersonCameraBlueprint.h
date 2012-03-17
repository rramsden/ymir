#ifndef _FIRSTPERSONBLUEPRINT_H
#define _FIRSTPERSONBLUEPRINT_H

#include <CCSBasicCameraModes.h>

#include "CameraModeBlueprint.h"

namespace Ymir {

    class FirstPersonCameraBlueprint : public CameraModeBlueprint {

        public: 
            FirstPersonCameraBlueprint() : CameraModeBlueprint() {
                mBlueprint["characterVisible"] = BPFP(NULL, (setFP)&setVisible);
            }

            ~FirstPersonCameraBlueprint(){}

            static void setVisible(CCS::FirstPersonCameraMode* mode,
                    boost::any& val)
            {
                bool vis = boost::any_cast<bool>(val);

                mode->setCharacterVisible(vis);
            }

        protected:
            CCS::FirstPersonCameraMode* createMode( std::string& id, 
                    PropList& props, 
                    CCS::CameraControlSystem* ccs)
            {
               return new CCS::FirstPersonCameraMode(
                       ccs, Ogre::Vector3(0,0,0), Ogre::Quaternion(1,0,0,0) );
            }
    };
}

#endif
