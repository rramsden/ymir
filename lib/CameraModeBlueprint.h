#ifndef _CAMERAMODEBLUEPRINT_H
#define _CAMERAMODEBLUEPRINT_H

#include <CCSCameraControlSystem.h>

#include "OgreBlueprint.h"

namespace Ymir {

    typedef CCS::CameraControlSystem::CameraMode CameraMode;

    class CameraModeBlueprint : public OgreBlueprint {
        public:
      
            CameraModeBlueprint() : OgreBlueprint(){}
            ~CameraModeBlueprint(){}

            //Meta blueprints, nothing actually created.
            //This is done for us in CameraBlueprint 
            void create(std::string&, PropList&){}
            void update(std::string&, PropList&){}
            void destroy(std::string&, PropList&){}

            //Set function is protected in ObjectBlueprint
            friend class CameraBlueprint;

        protected:

            virtual CameraMode* createMode(std::string&, PropList&, 
                    CCS::CameraControlSystem*) = 0;
    };
}

#endif
