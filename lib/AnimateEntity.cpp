#include <OgreMath.h>

#include "Core.h"
#include "AnimateEntity.h"

namespace Ymir {
    AnimateEntity::AnimateEntity(std::string& id) :
        mID(id),
        mScene(NULL),
        mNode(NULL),
        mObject(NULL),
        mCameraNode(NULL),
        mCameraPivot(NULL),
        mCameraGoal(NULL),
        mPosition(0,0,0),
        mGoalPosition(0,0,0),
        mMoveSpeed(0),
        mTurnSpeed(0),
        mAnimationCount(0),
        mAnimationFadeSpeed(0),
        mAnimations(),
        mAnimationFadeIn(),
        mAnimationFadeOut()
    {

    }

    AnimateEntity::~AnimateEntity() {

    }

    void AnimateEntity::update(Ogre::Real dt){

        //Update physical entity
        updateEntity(dt);

        //Continue playing active animations
        updateAnimations(dt); 

        if( mCameraNode ){
            updateCamera(dt);
        }
    }

    void AnimateEntity::updateEntity(Ogre::Real dt){
    
        Core::getSingletonPtr()->logNormal("position:  " + StringConverter::toString(mPosition) +
                                           ", goal: " + StringConverter::toString(mGoalPosition));

        //If we haven't reached our destination move 
        if( (mGoalPosition.x != mPosition.x) ||
            (mGoalPosition.z != mPosition.z) )
        {

            Quaternion toGoal = 
                mNode->getOrientation().zAxis().getRotationTo(mGoalPosition);


            Real yawToGoal = toGoal.getYaw().valueDegrees();
            Ogre::Real yawAtSpeed = 0; 

            if( yawToGoal && yawToGoal && dt && mTurnSpeed ){
                yawAtSpeed = yawToGoal / Math::Abs(yawToGoal) * dt * mTurnSpeed;
            }

            if( yawToGoal < 0 ){
                yawToGoal = std::min<Ogre::Real>(0, std::max<Ogre::Real>(yawToGoal, yawAtSpeed));
            } else if( yawToGoal > 0 ){
                yawToGoal = std::max<Ogre::Real>(0, std::min<Ogre::Real>(yawToGoal, yawAtSpeed));
            }

            mNode->yaw(Degree(yawToGoal));

            mNode->translate(0,0, dt * mMoveSpeed, Ogre::Node::TS_LOCAL);

            //Update stored position
            mPosition = mNode->getPosition();   

            //If we moved passed our mark, put ourselves at the dest
            if( mPosition > mGoalPosition ){
                mPosition = mGoalPosition;
                mNode->setPosition(mGoalPosition);
            }
        }

    }

    void AnimateEntity::updateCamera(Ogre::Real dt){

        //Place the camera 
        mCameraPivot->setPosition(mNode->getPosition() + Vector3::UNIT_Y * 2);

        Vector3 offset = mCameraGoal->_getDerivedPosition() - mCameraNode->getPosition();
        mCameraNode->translate(offset * dt * 9.0f);        

        mCameraNode->lookAt(mCameraPivot->_getDerivedPosition(), Ogre::Node::TS_WORLD);
    }

    void AnimateEntity::updateAnimations(Ogre::Real dt){

        //Walk the list of animations and add time to all enabled.
        std::map<std::string, Ogre::AnimationState*>::iterator it = 
            mAnimations.begin();

        for(; it != mAnimations.end(); it++){

            if( it->second->getEnabled() ){
                it->second->addTime(dt);
            }

            if( it->second->hasEnded() ){
                Core::getSingletonPtr()->logNormal("Animation " + it->first + " ended!");
                //TODO: Notify Erly bird
            }
        }

        //Fade in/out animations
        fadeAnimations(dt);
    }

    void AnimateEntity::fadeAnimations(Ogre::Real dt){
       std::map<std::string, Ogre::AnimationState*>::iterator it = 
           mAnimations.begin();

        for(; it != mAnimations.end(); it++){
            std::string id = it->first;
            Ogre::AnimationState* anim = it->second;

            if(mAnimationFadeIn[id]){

                Ogre::Real weight = anim->getWeight() + dt * mAnimationFadeSpeed;
                anim->setWeight(Ogre::Math::Clamp<Ogre::Real>(weight, 0, 1));

                Core::getSingletonPtr()->logNormal("Fading in " + id);

                if( weight >= 1 ){
                    mAnimationFadeIn[id] = false;
                }

            } else if( mAnimationFadeOut[id] ){

                Ogre::Real weight = anim->getWeight() - dt * mAnimationFadeSpeed;
                anim->setWeight(Ogre::Math::Clamp<Ogre::Real>(weight, 0, 1));

                Core::getSingletonPtr()->logNormal("Fading out " + id);

                if( weight <= 0 ){
                    anim->setEnabled(false);
                    mAnimationFadeOut[id] = false;
                }
            }
        }
    }
}
