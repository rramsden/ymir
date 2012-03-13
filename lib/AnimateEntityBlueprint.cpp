#include "AnimateEntityBlueprint.h"

#include "Core.h"

namespace Ymir {

AnimateEntityBlueprint::AnimateEntityBlueprint() : OgreBlueprint(){

    mBlueprint["mesh"] = BPFP(&decodeString, NULL);
    mBlueprint["position"] = BPFP(&decodeVector3, (setFP)&setPosition);
    mBlueprint["move"] = BPFP(&decodeVector3, (setFP)&setMove);
    mBlueprint["moveTo"] = BPFP(&decodeVector3, (setFP)&setMoveTo);
    mBlueprint["camera"] = BPFP(&decodeString, (setFP)&setCamera);
    mBlueprint["cameraGoal"] = BPFP(&decodeVector3, (setFP)&setCameraGoal);
    mBlueprint["cameraDistance"] = BPFP(&decodeReal, (setFP)&setCameraZoom);

    mBlueprint["moveSpeed"] = BPFP(&decodeReal, (setFP)&setMoveSpeed);
    mBlueprint["turnSpeed"] = BPFP(&decodeReal, (setFP)&setTurnSpeed);

    mBlueprint["animationFadeSpeed"] = 
        BPFP(&decodeFloat, (setFP)&setAnimationFadeSpeed);
    
    mBlueprint["animations"] = 
        BPFP(&decodeAnimations, (setFP)&setAnimations); 
    
    mBlueprint["skeletalEntities"] = 
        BPFP(&decodeSkeletalEntities, (setFP)&setSkeletalEntities);
}

Entity* createEntity(std::string& id, PropList& props, SceneManager* scene){
    std::string mesh = "";

    if( !props.hasProperty<std::string>("mesh", &mesh) ){
        return NULL;
    }

    return scene->createEntity(id, mesh);
}

void AnimateEntityBlueprint::create(std::string& id, PropList& props ){
    Core* core = Core::getSingletonPtr();
    boost::any temp;
    SceneManager* scene = core->mScene;

    //Create the actual object
    Entity* obj = createEntity(id, props, scene); 
    if( !obj ){
        //<<HERE>> TODO: Throw exception
        return;
    }

    AnimateEntity* ent = new AnimateEntity(id);

    //Create nodes for a 3rd person camera
    ent->mNode = scene->getRootSceneNode()->createChildSceneNode(id + "_node");

    ent->mEntityNode = ent->mNode->createChildSceneNode(id + "_entity_node");
    ent->mObject = obj;
    ent->mEntityNode->attachObject(obj);
    
    ent->mCameraNode = ent->mNode->createChildSceneNode(id + "_camera_node");
    ent->mCameraPitchNode = ent->mCameraNode->createChildSceneNode(id + "_camera_ptich_node");
    ent->mCameraZoom = 10.0f;
    ent->mCameraNode->setPosition(0.0f, 0.0f, 10.0f);
    ent->mScene = scene;
   
    //Init all defined animation states
    AnimationStateSet* animations = obj->getAllAnimationStates();
    AnimationStateIterator it = animations->getAnimationStateIterator();
    while(it.hasMoreElements()){
        AnimationState* state = it.getNext();
        std::string name = state->getAnimationName();

        core->logNormal("Loading animation: " + name);

        state->setLoop(true); 
        ent->mAnimations[name] = state;
        ent->mAnimationFadeIn[name] = false;
        ent->mAnimationFadeOut[name] = false;
    }

    //Set the remaining properties
    set(ent, props);

    //Update the physics components
    updatePhysics(ent);


    //Lastly, register the AnimateEntity with the event manager
    EventManager::getSingletonPtr()->monitor(ent);
}

void AnimateEntityBlueprint::update( std::string& id, PropList& props ){

    //Locate the animate entity state associated with id
    AnimateEntity* ent = EventManager::getSingletonPtr()->removeEntity(id);

    if( ent ){
        set(ent, props);
        updatePhysics(ent);

        EventManager::getSingletonPtr()->monitor(ent);
    } else {
        //TODO: Exception
    }
}

void AnimateEntityBlueprint::destroy(std::string& id, PropList& props){

    AnimateEntity* ent = EventManager::getSingletonPtr()->removeEntity(id);

    if( ent ){
        delete ent;
    } else {
        //TODO: Exception 
    }
}

void AnimateEntityBlueprint::updatePhysics(AnimateEntity* ent){

}

/*btCollisionShape* AnimateEntityBlueprint::createPhysicsObject( Entity* ent,
                                                              PropList& props )
{
    btCollisionShape* out = NULL;
    BtOgre::AnimatedMeshToShapeConverter converter(ent);

    Core::getSingletonPtr()->logNormal(
                "Creating box shape around static,non-primitive entity!");
    out = converter.createBox();
    
    return out;
}*/

int AnimateEntityBlueprint::decodeSkeletalEntity( const char* data, 
                                                  int* idx,
                                                  SkeletalEntity* out )
{
    int count = 0;
    std::string node = "", id = "", mesh = "";

    if( ei_decode_tuple_header(data, idx, &count) ||
        (count != 3) ||
        Ymir::decodeString(data, idx, &node) ||
        Ymir::decodeString(data, idx, &id)   ||
        Ymir::decodeString(data, idx, &mesh) )
    {
        return -EINVAL;
    }

    out->node = node;
    out->id = id;
    out->mesh = mesh;

    return 0;
}

int AnimateEntityBlueprint::decodeSkeletalEntities( const char* data,
                                                    int* idx, 
                                                    boost::any* out )
{
    std::list<SkeletalEntity> ents;

    if( decodeList<SkeletalEntity>(data, idx, &decodeSkeletalEntity, &ents) ){
        return -EINVAL;
    }

    *out = ents;

    return 0;
}

int AnimateEntityBlueprint::decodeAnimations( const char* data,
                                              int* idx, 
                                              boost::any* out )
{
    std::list<std::string> temp;

    if( decodeList<std::string>(data, idx, &Ymir::decodeString, &temp) ){
        return -EINVAL;
    }

    *out = temp;

    return 0;
}

void AnimateEntityBlueprint::setSkeletalEntities(AnimateEntity* ent, 
                                                 boost::any& ents)
{
    SceneManager* scene = ent->mScene;
    Entity* entity = ent->mObject;
    std::list<SkeletalEntity> nodes = 
        boost::any_cast<std::list<SkeletalEntity> >(ents);

    std::list<SkeletalEntity>::iterator it = nodes.begin();
    for(; it != nodes.end(); it++){
        Entity* skEnt = NULL;

        try{
            skEnt = scene->getEntity(it->id);
        } catch(...){
            skEnt = scene->createEntity(it->id, it->mesh);
        }
      
        try {
            entity->detachObjectFromBone(it->id);
        } catch(...){
            //Nothing to worry about
        }

        entity->attachObjectToBone(it->node, skEnt);
    }

    //TODO: Recalculate physics object
}

void AnimateEntityBlueprint::setPosition(AnimateEntity* ent,
                                        boost::any& pos)
{
    Vector3 vPos = boost::any_cast<Vector3>(pos);

    ent->mNode->setPosition(vPos);

    ent->mPosition = vPos;
    ent->mGoalPosition = vPos;
}

void AnimateEntityBlueprint::setMove(AnimateEntity* ent,
                                     boost::any& goal)
{
    ent->mGoalPosition += boost::any_cast<Ogre::Vector3>(goal);
}

void AnimateEntityBlueprint::setMoveTo(AnimateEntity* ent,
                                       boost::any& pos)
{
    Ogre::Vector3 temp = boost::any_cast<Ogre::Vector3>(pos);

    ent->mGoalPosition = Vector3(temp.x, ent->mGoalPosition.y, temp.z);
}

void AnimateEntityBlueprint::setMoveSpeed(AnimateEntity* ent,
                                          boost::any& sp)
{
    ent->mMoveSpeed = boost::any_cast<Ogre::Real>(sp);
}

void AnimateEntityBlueprint::setTurnSpeed(AnimateEntity* ent,
                                          boost::any& ts)
{
    ent->mTurnSpeed = boost::any_cast<Ogre::Real>(ts);
}

void AnimateEntityBlueprint::setCamera(AnimateEntity* ent, 
                                       boost::any& id)
{
    std::string camID = boost::any_cast<std::string>(id);
    Ogre::Camera* cam = ent->mScene->getCamera(camID);
    Ogre::SceneNode* camNode = cam->getParentSceneNode();

    //Detach the camera from its previous node
    camNode->detachObject(cam);

    //This camera is now ours
    ent->mCameraPitchNode->attachObject(cam);

    //Reset the position of camera, otherwise its offset 
    //will affect desired behavior
    cam->setPosition(Vector3(0,0,0));
    cam->setAutoTracking(true, ent->mNode);

    //Place the camera about the character
    //ent->mCameraNode->setPosition(mNode->getPosition());
    //ent->mCameraNode->translate(Vector3(0.0f, 0.0f, 10.0f));

    //Camera node will track the pivot as it orbits the character
    //ent->mCameraNode->setFixedYawAxis(true);
    //ent->mCameraNode->setAutoTracking(true, ent->mCameraPivot);
}

void AnimateEntityBlueprint::setCameraGoal(AnimateEntity* ent,
                                           boost::any& vec)
{
    /*Ogre::Vector3 delta = boost::any_cast<Ogre::Vector3>(vec);
    Ogre::Real dYaw = 0.0f, dPitch = 0.0f, dZoom = 0.0f;
    Ogre::Vector3 camPos = ent->mCameraNode->_getDerivedPosition();
    Ogre::Quaternion camOrien = ent->mCameraNode->_getDerivedOrientation();

    //Yaw is unbounded
    dYaw = (ent->mCameraYaw += delta.y);

    if( ((ent->mCameraPitch + delta.x) <= 25) &&
        ((ent->mCameraPitch + delta.x) >= -60) )
    {
        dPitch = (ent->mCameraPitch += delta.x);
    }

    //Bound zoom
    if( ((ent->mCameraZoom + delta.z) >= 8) &&
        ((ent->mCameraZoom + delta.z) <= 25) )
    {
        dZoom = (ent->mCameraZoom += delta.z);
    }

    Ogre::Quaternion offsetPitch(Degree(dPitch), Ogre::Vector3::UNIT_X);
    Ogre::Quaternion offsetYaw(Degree(dYaw), Ogre::Vector3::UNIT_Y);
    Vector3 offsetZoom(0,0, dZoom);
   
    //Update camera
    ent->mCameraNode->setPosition(
            camPos + (camOrien * offsetPitch * offsetYaw * offsetZoom));
    ent->mCameraNode->setOrientation(camOrien * offsetPitch * offsetYaw);*/

    /*Vector3 rotCenter = ent->mEntityNode->getPosition();
    Vector3 diffPos = ent-> mCameraNode->getPosition() - rotCenter;
    Ogre::Vector3 delta = boost::any_cast<Ogre::Vector3>(vec);
    
    Ogre::Vector3 nodePos = ent->mNode->_getDerivedPosition();
    Ogre::Vector3 camPos = ent->mCameraNode->_getDerivedPosition();
    Ogre::Real dist = nodePos.distance(camPos);
    Ogre::Real distChange = boost::any_cast<Ogre::Real>(delta.z) * dist;

    Ogre::Quaternion qt = ent->mCameraPitchNode->getOrientation();
    Ogre::Quaternion et = ent->mEntityNode->getOrientation();
    Ogre::Real pitch = qt.getPitch().valueDegrees();

    //Yaw is unbounded
    Vector3 temp = (Quaternion(Degree(delta.y), Vector3::UNIT_Y) + Quaternion(Degree(delta.x), Vector3::UNIT_X)) * diffPos;
    ent->mCameraNode->setPosition(temp + rotCenter);

    //Bound pitch
    if( !(((pitch + delta.x) > 25) && (delta.x > 0)) &&
        !(((pitch + delta.x) < -60) && (delta.x < 0)) ) 
    {
        Core::getSingletonPtr()->logNormal("Updating Camera Goal:  "
                                           "Pitch: " + StringConverter::toString(pitch) + 
                                           ", Delta: " + StringConverter::toString(delta.x) + 
                                           ", New Goal: " + StringConverter::toString((pitch + delta.x)));
        
        Vector3 temp2 = Quaternion(Degree(delta.x), Vector3::UNIT_X) * diffPos;
        ent->mCameraNode->setPosition(temp2 + rotCenter);
    }*/


}


void AnimateEntityBlueprint::setCameraZoom(AnimateEntity* ent,
                                            boost::any& real)
{
    /*Ogre::Vector3 goalPos = ent->mCameraGoal->_getDerivedPosition();
    Ogre::Vector3 pivPos = ent->mCameraPivot->_getDerivedPosition();
    Ogre::Real dist = goalPos.distance(pivPos);
    Ogre::Real distChange = boost::any_cast<Ogre::Real>(real) * dist;

    //Bound zoom
    if( !(dist + distChange < 8 && distChange < 0) &&
        !(dist + distChange > 25 && distChange > 0) )
    {
        ent->mCameraGoal->translate(0, 0, distChange, Ogre::Node::TS_LOCAL);
    }*/
}

void AnimateEntityBlueprint::setAnimationFadeSpeed(AnimateEntity* ent,
                                                   boost::any& speed)
{
    ent->mAnimationFadeSpeed = boost::any_cast<float>(speed);
}

void AnimateEntityBlueprint::setAnimations(AnimateEntity* ent,
                                           boost::any& set)
{
    std::list<std::string> anims = 
        boost::any_cast<std::list<std::string> >(set);

    Core::getSingletonPtr()->logNormal("Setting Animations!");

    //Fade all currently active animations
    std::map<std::string, Ogre::AnimationState*>::iterator it = 
        ent->mAnimations.begin();

    for(; it != ent->mAnimations.end(); it++ ){
        std::string id = it->first;
        Ogre::AnimationState* anim = it->second;

        if( anim->getEnabled() || ent->mAnimationFadeIn[id] ){
            
            ent->mAnimationFadeIn[id] = false;
            ent->mAnimationFadeOut[id] = true;
        }
    }

    //Fade in the animations listed
    std::list<std::string>::iterator animIT = anims.begin();
    for(; animIT != anims.end(); animIT++){
        std::string id = *(animIT);
        Ogre::AnimationState* anim = ent->mAnimations[id];

        Core::getSingletonPtr()->logNormal("Starting: " + id);
        ent->mAnimationFadeIn[id] = true;
        anim->setEnabled(true);
        anim->setWeight(0);
        anim->setTimePosition(0);
    }
}

}
