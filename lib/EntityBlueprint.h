#ifndef _ENTITYBLUPRINT_H
#define _ENTITYBLUPRINT_H

#include <OgreEntity.h>

#include "NodeBlueprint.h"

namespace Ymir {

    class EntityBlueprint : public NodeBlueprint<Ogre::Entity> {

        public:
            EntityBlueprint();
            ~EntityBlueprint(){}
        protected:

            Ogre::Entity* createOgreObject(std::string&,
                                     Ymir::PropList&,
                                     Ogre::SceneManager*);

            Ogre::Entity* findOgreObject( std::string& id, 
                                          Ogre::SceneManager* scene);

            void destroyOgrebject( std::string&, Ogre::SceneManager* );

    };
}
#endif
