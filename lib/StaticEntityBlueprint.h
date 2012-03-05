#ifndef _STATICENTITYBLUPRINT_H
#define _STATICENTITYBLUPRINT_H

#include <OgreEntity.h>

#include "NodeBlueprint.h"

namespace Ymir {

    class StaticEntityBlueprint : public NodeBlueprint<Ogre::Entity> {

        public:
            StaticEntityBlueprint();
            ~StaticEntityBlueprint(){}

            static int decodePrefabType(const char*, int*, boost::any*);

            static void setMaterial( NodeInfo<Ogre::Entity>* t, boost::any& );

        protected:

            Ogre::Entity* createOgreObject(std::string&,
                                     Ymir::PropList&,
                                     Ogre::SceneManager*);

            btCollisionShape* createPhysicsObject( Entity* ent, PropList& props );

            Ogre::Entity* findOgreObject( std::string& id, 
                                          Ogre::SceneManager* scene);

            void destroyOgreObject( std::string&, Ogre::SceneManager* );

    };
}
#endif
