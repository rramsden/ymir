#include <OgreMath.h>

#include "Core.h"
#include "AnimateEntity.h"

namespace Ymir {
    AnimateEntity::AnimateEntity(std::string& id) :
        mID(id),
        mScene(NULL),
        mNode(NULL),
        mEntityNode(NULL),
        mObject(NULL),
        mVelocity(0),
        mVelocityMax(0),
        mAcceleration(0),
        mAccelerationFactor(0),
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
    }

    void AnimateEntity::updateEntity(Ogre::Real dt){

        if( mAccelerationFactor){  //AccelerationFactor != 0 means we are accelerating
            Real newVelocity = mVelocity + (mAccelerationFactor *  mAcceleration * dt);
            Real sign = newVelocity / Math::Abs(newVelocity);



            mVelocity = sign * std::min<Real>( mVelocityMax, Math::Abs(newVelocity) );

        } else {
            mVelocity = std::max<Real>(0, mVelocity - (mAcceleration * dt));
        }

        //Move back/forth relative to current velocity
        Vector3 orienVec = mNode->getOrientation().zAxis().normalisedCopy();
        mNode->translate( orienVec * (mVelocity * dt) );

        /*mPosition = mNode->getPosition(); 

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
                yawToGoal = std::min<Ogre::Real>(0.0f, std::max<Ogre::Real>(yawToGoal, yawAtSpeed));
            } else if( yawToGoal > 0 ){
                yawToGoal = std::max<Ogre::Real>(0.0f, std::min<Ogre::Real>(yawToGoal, yawAtSpeed));
            }

            mNode->yaw(Degree(yawToGoal));


      Core::getSingletonPtr()->logNormal("position:  " + StringConverter::toString(mPosition) +

                                           ", goal: " + StringConverter::toString(mGoalPosition) + 
                                           ", yawToGoal: " + StringConverter::toString(yawToGoal));

            if( Math::RealEqual(yawToGoal, 0.0f, .00001f) ){
           
               mNode->setPosition(mGoalPosition);
            mNode->translate(0,0, dt * mMoveSpeed, Ogre::Node::TS_LOCAL);

            //Update stored position
            mPosition = mNode->getPosition();  

            Ogre::Real dist = Math::Abs((mGoalPosition - mPosition).length());
            Ogre::Real move = dt * mMoveSpeed;

            Core::getSingletonPtr()->logNormal("\tdist: " + StringConverter::toString(dist) + ", move: " + StringConverter::toString(move));

            if( dist >= move ){
                mNode->translate(0,0, dt * mMoveSpeed, Ogre::Node::TS_LOCAL);
            } else {
                mNode->translate(0,0, dist, Ogre::Node::TS_LOCAL);
            }
            }
        }*/
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
                    anim->setTimePosition(0);
                    anim->setWeight(0);
                    mAnimationFadeOut[id] = false;
                }
            }
        }
    }
}
