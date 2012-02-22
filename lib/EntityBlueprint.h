#ifndef _ENTITYBLUPRINT_H
#define _ENTITYBLUPRINT_H

#include <OgreEntity.h>

#include "NodeBlueprint.h"

namespace Ymir {

    class EntityBlueprint : public NodeBlueprint<Ogre::Entity> {

        public:
            EntityBlueprint();
            ~EntityBlueprint(){}

            static int decodePrefabType(const char*, int*, boost::any*);

            static void setMaterial( NodeInfo<Ogre::Entity>* t, boost::any& );

        protected:

            Ogre::Entity* createOgreObject(std::string&,
                                     Ymir::PropList&,
                                     Ogre::SceneManager*);

            btCollisionShape* createPhysicsObject( Entity* ent );

            Ogre::Entity* findOgreObject( std::string& id, 
                                          Ogre::SceneManager* scene);

            void destroyOgreObject( std::string&, Ogre::SceneManager* );

    };
}
#endif
