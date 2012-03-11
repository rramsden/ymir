#ifndef _ANIMATEENTITYBLUEPRINT_H
#define _ANIMATEENTITYBLUEPRINT_H

#include <OgreEntity.h>

#include "OgreBlueprint.h"
#include "AnimateEntity.h"


namespace Ymir {

    typedef struct {
        std::string node;
        std::string id;
        std::string mesh;
    } SkeletalEntity;
    
    class AnimateEntityBlueprint : public OgreBlueprint {

        public:
            AnimateEntityBlueprint();
            ~AnimateEntityBlueprint(){}

            void create(std::string& id, PropList& props);
            void update(std::string& id, PropList& props);
            void destroy(std::string& id, PropList& props);

            static int decodeSkeletalEntity( const char* data,
                                             int* idx, 
                                             SkeletalEntity* out );

            static int decodeSkeletalEntities( const char* data, 
                                               int* idx, 
                                               boost::any* out );

            static int decodeAnimations( const char* data, 
                                         int* idx, 
                                         boost::any* out );

            static void setPosition(AnimateEntity*, boost::any&);
            static void setCamera(AnimateEntity*, boost::any&);
            static void setCameraGoal(AnimateEntity*, boost::any&);
            static void setCameraZoom(AnimateEntity*, boost::any&);

            static void setMove(AnimateEntity*, boost::any&);
            static void setMoveSpeed(AnimateEntity*, boost::any&);
            static void setTurnSpeed(AnimateEntity*, boost::any&);

            static void setSkeletalEntities(AnimateEntity*, boost::any&);
            static void setAnimationFadeSpeed(AnimateEntity*, boost::any&);
            static void setAnimations(AnimateEntity*, boost::any&);

        protected:

            void updatePhysics( AnimateEntity* ent );
    };

}
#endif
