#include "CameraBlueprint.h"

namespace Ymir {

        CameraBlueprint::CameraBlueprint() : NodeBlueprint<Ogre::Camera>() {
        
        //Empty inherited node setters
        mBlueprint.empty();

        mBlueprint.insert( 
            BPEntry("position", BPFP(&decodeVector3, (setFP)&setCameraPosition)) );

        mBlueprint.insert(
            BPEntry("move", BPFP(&decodeVector3, (setFP)&setCameraMove)));

        mBlueprint.insert(
            BPEntry("direction", BPFP(&decodeVector3, (setFP)&setCameraDirection)) );
        
        mBlueprint.insert(
            BPEntry("yaw", BPFP(&decodeRadian, (setFP)&setCameraYaw)) );

        mBlueprint.insert(
            BPEntry("pitch", BPFP(&decodeRadian, (setFP)&setCameraPitch)) );

        mBlueprint.insert(
            BPEntry("roll", BPFP(&decodeRadian, (setFP)&setCameraRoll)) );

        mBlueprint.insert(
            BPEntry("lookAt", BPFP(&decodeVector3, (setFP)&setCameraLookAt)) );

        mBlueprint.insert(
            BPEntry("nearClip", BPFP(&decodeReal, (setFP)&setNearClip)) );

        mBlueprint.insert(
            BPEntry("farClip", BPFP(&decodeReal, (setFP)&setFarClip)) );

        mBlueprint.insert(
            BPEntry("fixYaw", BPFP(&decodeBool, (setFP)&setFixYaw)) );

    }

    //Setters
    void CameraBlueprint::setCameraPosition(NodeTuple<Ogre::Camera>* t, boost::any& pos){
        t->mObject->setPosition(boost::any_cast<Vector3>(pos));
    }
    
    void CameraBlueprint::setCameraMove(NodeTuple<Ogre::Camera>* t, boost::any& diff){
        t->mObject->moveRelative( boost::any_cast<Ogre::Vector3>(diff) );
    }
    
    void CameraBlueprint::setCameraDirection(NodeTuple<Ogre::Camera>* t, boost::any& dir){
        t->mObject->setDirection(boost::any_cast<Vector3>(dir));    
    }
    
    void CameraBlueprint::setCameraYaw(NodeTuple<Ogre::Camera>* t, boost::any& rad){
        t->mObject->yaw(boost::any_cast<Radian>(rad));
    }
    
    
    void CameraBlueprint::setCameraPitch(NodeTuple<Ogre::Camera>* t, boost::any& rad){
        t->mObject->pitch(boost::any_cast<Radian>(rad));
    }
    
    
    void CameraBlueprint::setCameraRoll(NodeTuple<Ogre::Camera>* t, boost::any& rad){
        t->mObject->roll(boost::any_cast<Radian>(rad));
    }
    
    void CameraBlueprint::setCameraLookAt(NodeTuple<Ogre::Camera>* t, boost::any& look){
        t->mObject->lookAt(boost::any_cast<Vector3>(look));
    }

    void CameraBlueprint::setNearClip(NodeTuple<Ogre::Camera>* t, boost::any& val){
        t->mObject->setNearClipDistance(boost::any_cast<Ogre::Real>(val));
    }

    void CameraBlueprint::setFarClip(NodeTuple<Ogre::Camera>* t, boost::any& val){
        t->mObject->setFarClipDistance(boost::any_cast<Ogre::Real>(val));
    }

    void CameraBlueprint::setFixYaw(NodeTuple<Ogre::Camera>* t, boost::any& val){
        t->mObject->setFixedYawAxis(boost::any_cast<bool>(val));
    }

    Ogre::Camera* CameraBlueprint::createOgreObject( std::string& id, 
                                                     PropList& props, 
                                                     Ogre::SceneManager* scene)
    {
        return scene->createCamera(id);
    }


    Ogre::Camera* CameraBlueprint::findOgreObject( std::string& id, 
                                                   Ogre::SceneManager* scene)
    {
        return scene->getCamera(id);
    }

    void CameraBlueprint::destroyOgreObject( std::string& id, 
                                             Ogre::SceneManager* scene)
    {
        scene->destroyCamera(id);
    }

}   
