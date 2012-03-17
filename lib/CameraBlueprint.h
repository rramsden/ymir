#ifndef _CAMERABLUEPRINT_H
#define _CAMERABLUEPRINT_H

#include <CCSCameraControlSystem.h>

#include "ObjectBlueprint.h"

//Mode blueprints
#include "FreeCameraBlueprint.h"
#include "FirstPersonCameraBlueprint.h"
#include "RTSCameraBlueprint.h"
#include "OrbitalCameraBlueprint.h"

namespace Ymir {

    class CameraBlueprint : public OgreBlueprint {

        public:
            CameraBlueprint();
            ~CameraBlueprint(){}
           
            void create(std::string&, PropList&);
            void update(std::string&, PropList&);
            void destroy(std::string&, PropList&);

            static int decodeType(const char*, int*, boost::any*);
       
            static void setNearClip(CCS::CameraControlSystem*, boost::any&);
            static void setFarClip(CCS::CameraControlSystem*, boost::any&);
            static void setFixYaw(CCS::CameraControlSystem*, boost::any&);
            static void setTarget(CCS::CameraControlSystem*, boost::any&);
            static void setAutoTrack(CCS::CameraControlSystem*, boost::any&);
            static void setPosition(CCS::CameraControlSystem*, boost::any&);
            static void setLookAt(CCS::CameraControlSystem*, boost::any&);

        protected:
           typedef enum {
                CCS_FREE = 0,
                CCS_FIRSTPERSON,
                CCS_RTS, 
                CCS_ORBIT,
                CCS_MAX
            } CCSType;

            FreeCameraBlueprint mFreeBp;
            FirstPersonCameraBlueprint mFirstPersonBp;
            RTSCameraBlueprint mRTSBp;
            OrbitalCameraBlueprint mOrbitalBp;

            typedef std::pair<CameraModeBlueprint*, CameraMode*> CameraState;
            typedef std::map<std::string, CameraState> CameraStates;
            CameraStates mCameraStates; 

            CameraModeBlueprint* mBPS[CCS_MAX];
    };
}
#endif
