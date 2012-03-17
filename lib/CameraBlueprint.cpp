//CCS Camera types
#include <CCSBasicCameraModes.h>
#include <CCSFreeCameraMode.h>
#include <CCSOrbitalCameraMode.h>

#include "Core.h"
#include "CameraBlueprint.h"

namespace Ymir {

        //Helper table to lookup CCS blueprints by type
        //ORDERING MATTERS HERE 
        /*CameraModeBlueprint* bps[CCS_MAX] = {
            &mFreeBp,
            &mFirstPersonBp,
            &mRTSBp,
            &mOrbitalBp
        }*/ 
         
        CameraBlueprint::CameraBlueprint() :
            OgreBlueprint(), 
            mFreeBp(),
            mFirstPersonBp(),
            mRTSBp(),
            mOrbitalBp(),
            mCameraStates() 
        {

            //Setup static camera mode blueprint table
            mBPS[CCS_FREE] = &mFreeBp;
            mBPS[CCS_FIRSTPERSON] = &mFirstPersonBp;
            mBPS[CCS_RTS] =  &mRTSBp;
            mBPS[CCS_ORBIT] = &mOrbitalBp;

            //Required properties for creation
            mBlueprint["type"] = BPFP(&decodeType, NULL);

            //Optional properties
            mBlueprint["nearClip"] = BPFP(&decodeReal, (setFP)&setNearClip);
            mBlueprint["farClip"] = BPFP(&decodeReal, (setFP)&setFarClip);
            mBlueprint["target"] = BPFP(&decodeString, (setFP)&setTarget);
            mBlueprint["fixYaw"] = BPFP(&decodeBool, (setFP)&setFixYaw);
            mBlueprint["autoTrack"] = BPFP(&decodeBool, (setFP)&setAutoTrack);
            mBlueprint["position"] = BPFP(&decodeVector3, (setFP)&setPosition);
            mBlueprint["lookAt"] = BPFP(&decodeVector3, (setFP)&setLookAt);

            //Properties which are specific to supported 
            //camera modes.  These must occur here as decoding
            //is not inheritance aware.  Setting of these properties
            //MUST occur within CSSBlueprints.

            mBlueprint["move"] = BPFP(&decodeVector3, NULL);
            mBlueprint["moveSpeed"] = BPFP(&decodeReal, NULL);
            mBlueprint["rotate"] = BPFP(&decodeVector3, NULL);
            mBlueprint["rotateSpeed"] = BPFP(&decodeReal, NULL);

            mBlueprint["characterVisible"] = BPFP(&decodeBool, NULL);
            mBlueprint["zoom"] = BPFP(&decodeReal, NULL);
            mBlueprint["zoomSpeed"] = BPFP(&decodeReal, NULL);
            mBlueprint["minZoom"] = BPFP(&decodeReal, NULL);
            mBlueprint["maxZoom"] = BPFP(&decodeReal, NULL);
            /*mBlueprint["yaw"] = BPFP(&decodeRadian, NULL);
            mBlueprint["pitch"] = BPFP(&decodeRadian, NULL);
            mBlueprint["roll"] = BPFP(&decodeRadian, (setFP)&setCameraRoll);*/
    }

    void CameraBlueprint::create(std::string& id, PropList& props){
        Core* core = Core::getSingletonPtr();
        CCS::CameraControlSystem* ccs = core->mCCS;
        CCSType type; 

        if( !ccs ){
            core->logCritical("Camera control system not initialized!"
                              "A scene must be created first!");
            //TODO Raise exception
            return;
        } 

        if( ccs->getCameraMode(id) ){
            core->logCritical("Camera mode " + id + " already exists!");
            //TODO Raise exception
            return;
        }

        if( !props.hasProperty<CCSType>("type", &type) ){
            core->logCritical("No camera type defined!");
            //TODO: Raise exception;
            return;
        }

        //Have the specific camera create themselves
        CameraModeBlueprint* bp = mBPS[type]; 
        CameraMode* mode = bp->createMode(id, props, ccs);

        //Register the new mode
        ccs->registerCameraMode(id, mode); 

        //Call our setters (effect CCS)
        set(ccs, props);

        //Call CCS blueprint setters for listed properties
        bp->set(mode, props);

        //Lastly, record relevant data for later lookup
        mCameraStates[id] = CameraState(bp, mode);  
    }

    void CameraBlueprint::update(std::string& id, PropList& props){
        Core* core = Core::getSingletonPtr();
        CCS::CameraControlSystem* ccs = core->mCCS;
        CameraMode* mode = NULL; 
         
        if( !ccs || !(mode = ccs->getCameraMode(id)) ){
            core->logCritical("Camera mode " + id + " does not exist!");
            //TODO: Raise Exception
            return;
        }

        CameraState state = mCameraStates[id];   

        //Update indicated properties 
        state.first->set(mode, props);
    }

    void CameraBlueprint::destroy(std::string& id, PropList& props){
        Core* core = Core::getSingletonPtr();
        CCS::CameraControlSystem* ccs = core->mCCS;
        CameraMode* mode = NULL;
    
        if( !ccs || !(mode = ccs->getCameraMode(id)) ){
            core->logCritical("Camera mode " + id + " does not exist!");
            //TODO: Raise Exception
            return;
        }

        //Remove the camera mode from CCS
        ccs->removeCameraMode(mode);

        //Erase our stored state
        mCameraStates.erase( mCameraStates.find(id) );
    }    

    //Decoders
    int CameraBlueprint::decodeType( const char* data,
                                     int* idx, 
                                     boost::any* out )
    {
        std::string temp = "";

        if( Ymir::decodeString(data, idx, &temp) ){
            return -EINVAL;
        }
        
        if( temp == "free" ){
            *out = CCS_FREE;
        } else if( temp == "first_person" ){
            *out = CCS_FIRSTPERSON;
        } else if( temp == "rts" ){
            *out = CCS_RTS;
        } else if( temp == "orbit" ){
            *out = CCS_ORBIT;
        } else {
            *out = CCS_FREE;
        }

        return 0;
    }

    //Setters
    /*void CameraBlueprint::setCameraPosition(NodeInfo<Ogre::Camera>* t, boost::any& pos){
        t->mObject->setPosition(boost::any_cast<Vector3>(pos));
    }
    
    void CameraBlueprint::setCameraMove(NodeInfo<Ogre::Camera>* t, boost::any& diff){
        
        t->mObject->moveRelative( boost::any_cast<Ogre::Vector3>(diff) );
    }
    
    void CameraBlueprint::setCameraDirection(NodeInfo<Ogre::Camera>* t, boost::any& dir){
        t->mObject->setDirection(boost::any_cast<Vector3>(dir));    
    }
    
    void CameraBlueprint::setCameraYaw(NodeInfo<Ogre::Camera>* t, boost::any& rad){
        t->mObject->yaw(boost::any_cast<Radian>(rad));
    }
    
    
    void CameraBlueprint::setCameraPitch(NodeInfo<Ogre::Camera>* t, boost::any& rad){
        t->mObject->pitch(boost::any_cast<Radian>(rad));
    }
    
    
    void CameraBlueprint::setCameraRoll(NodeInfo<Ogre::Camera>* t, boost::any& rad){
        t->mObject->roll(boost::any_cast<Radian>(rad));
    }
    
    void CameraBlueprint::setCameraLookAt(NodeInfo<Ogre::Camera>* t, boost::any& look){
        t->mObject->lookAt(boost::any_cast<Vector3>(look));
    }*/

    void CameraBlueprint::setNearClip(CCS::CameraControlSystem* ccs, 
                                      boost::any& val)
    {
        Ogre::Real clip = boost::any_cast<Ogre::Real>(val);
        Ogre::Camera* cam = ccs->getOgreCamera(); 

        cam->setNearClipDistance(clip);
    }

    void CameraBlueprint::setFarClip(CCS::CameraControlSystem* ccs, 
                                     boost::any& val)
    {
        Ogre::Real clip = boost::any_cast<Ogre::Real>(val);
        Ogre::Camera* cam = ccs->getOgreCamera(); 

        cam->setFarClipDistance(clip);
    }

    void CameraBlueprint::setFixYaw(CCS::CameraControlSystem* ccs, 
                                    boost::any& val)
    {
        ccs->setFixedYawAxis(boost::any_cast<bool>(val));
    }

    void CameraBlueprint::setTarget(CCS::CameraControlSystem* ccs, 
                                    boost::any& val)
    {
        std::string id = boost::any_cast<std::string>(val);
        Ogre::SceneManager* sm = ccs->getSceneManager();
        //This may have to change eventually...
        Ogre::SceneNode* node = sm->getEntity(id)->getParentSceneNode();
    
        ccs->setCameraTarget(node);
    }
   
    void CameraBlueprint::setAutoTrack(CCS::CameraControlSystem* ccs, 
                                       boost::any& val)
    {
        ccs->setAutoTrackingTarget(boost::any_cast<bool>(val));
    }

    void CameraBlueprint::setPosition(CCS::CameraControlSystem* ccs,
                                      boost::any&val)
    {
        Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);
        ccs->getOgreCamera()->setPosition(vec);
    }

    void CameraBlueprint::setLookAt(CCS::CameraControlSystem* ccs,
                                    boost::any& val)
    {
        Ogre::Vector3 vec = boost::any_cast<Ogre::Vector3>(val);
        
        ccs->getOgreCamera()->lookAt(vec);
    }
}   
