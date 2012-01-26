#include "EntityBlueprint.h"

using namespace Ogre;

namespace Ymir {
EntityBlueprint::EntityBlueprint() : NodeBlueprint<Ogre::Entity>(){

    mBlueprint.insert( BPEntry("mesh", BPFP(decodeString, NULL)) );
}


Ogre::Entity* EntityBlueprint::createOgreObject(std::string& id,
                                                Ymir::PropList& props,
                                                Ogre::SceneManager* scene)
{
    boost::any temp;

    //Mesh must be defined otherwise throw exception
    if( props.hasProperty("mesh", &temp) ){
        std::string mesh = boost::any_cast<std::string>(temp);
        return scene->createEntity(id, mesh);
    } else {
        //<<HERE>> TODO: Throw exception
        return NULL;
    }
}

Ogre::Entity* EntityBlueprint::findOgreObject( std::string& id, 
                                               Ogre::SceneManager* scene){
    return scene->getEntity(id);
}

void EntityBlueprint::destroyOgrebject( std::string& id, 
                                        Ogre::SceneManager* scene){
   scene->destroyEntity(id); 
}
}
