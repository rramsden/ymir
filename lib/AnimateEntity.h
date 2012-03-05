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

            void updateAnimations(Ogre::Real dt);
            void fadeAnimations(Ogre::Real dt);

            std::string mID;
            
            //Ogre data
            Ogre::SceneManager* mScene;
            Ogre::SceneNode* mNode;
            Ogre::Entity* mObject;
            
            //Animation relevant data
            int mAnimationCount;
            int mAnimationFadeSpeed;
            std::map<std::string, Ogre::AnimationState*> mAnimations;
            std::map<std::string, bool> mAnimationFadeIn;
            std::map<std::string, bool> mAnimationFadeOut; 
    };
}
#endif
