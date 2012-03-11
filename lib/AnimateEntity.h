#ifndef _ANIMATEENTITY_H
#define _ANIMATEENTITY_H

#include <string>
#include <map>
#include <OgreEntity.h>
#include <OgreSceneNode.h>
#include <OgreAnimationState.h>

namespace Ymir {

    class AnimateEntity {

        public:

            ~AnimateEntity();

            void update(Ogre::Real dt);

            std::string getID(){return mID;}

            friend class AnimateEntityBlueprint;

        protected:
            AnimateEntity(std::string& id);
            
            void updateEntity(Ogre::Real dt);

            void updateAnimations(Ogre::Real dt);
            void fadeAnimations(Ogre::Real dt);

            void updateCamera(Ogre::Real dt);

            std::string mID;
            
            //Ogre data
            Ogre::SceneManager* mScene;
            Ogre::SceneNode* mNode;
            Ogre::Entity* mObject;
            
            //Camera data
            Ogre::SceneNode* mCameraNode;
            Ogre::SceneNode* mCameraPivot; 
            Ogre::SceneNode* mCameraGoal;

            //Movement Options
            Ogre::Vector3 mPosition; 
            Ogre::Vector3 mGoalPosition;
            Ogre::Real mMoveSpeed;
            Ogre::Real mTurnSpeed;

            //Animation relevant data
            int mAnimationCount;
            int mAnimationFadeSpeed;
            std::map<std::string, Ogre::AnimationState*> mAnimations;
            std::map<std::string, bool> mAnimationFadeIn;
            std::map<std::string, bool> mAnimationFadeOut; 
    };
}
#endif
