#include <OgreMath.h>

#include "Core.h"
#include "AnimateEntity.h"

namespace Ymir {
    AnimateEntity::AnimateEntity(std::string& id) :
        mID(id),
        mScene(NULL),
        mNode(NULL),
        mObject(NULL),
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

        //Continue playing active animations
        updateAnimations(dt); 
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
