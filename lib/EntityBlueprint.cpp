#include "EntityBlueprint.h"

using namespace Ogre;

namespace Ymir {
EntityBlueprint::EntityBlueprint() : NodeBlueprint<Ogre::Entity>(){

    mBlueprint["mesh"] = BPFP(&decodeString, NULL);
    mBlueprint["shape"] = BPFP(&decodePrefabType, NULL);
    mBlueprint["material"] = BPFP(&decodeString, (setFP)&setMaterial);
}

int EntityBlueprint::decodePrefabType( const char* data, 
                                   int* idx,
                                   boost::any* out )
{   
    string shape = "";

    if( Ymir::decodeString(data, idx, &shape) ){
        return -EINVAL;
    }
    
    if( shape == "plane" ){
        *out = Ogre::SceneManager::PT_PLANE;
    } else if( shape == "cube" ){
        *out = Ogre::SceneManager::PT_CUBE;
    } else if( shape == "sphere" ){
        *out = Ogre::SceneManager::PT_SPHERE;
    } else {
        return -EINVAL;
    }


    return 0;
}

void setMaterial( NodeTuple<Ogre::Entity>* t, boost::any& mat ){
    t->mObject->setMaterialName(boost::any_cast<std::string>(mat));
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
    } else if( props.hasProperty("shape", &temp) ) {
        Ogre::SceneManager::PrefabType type = 
            boost::any_cast<Ogre::SceneManager::PrefabType>(temp);        
        return scene->createEntity(id, type);
    } else {
        //<<HERE>> TODO: Throw exception
        return NULL;
    }
}

Ogre::Entity* EntityBlueprint::findOgreObject( std::string& id, 
                                               Ogre::SceneManager* scene){
    return scene->getEntity(id);
}

void EntityBlueprint::destroyOgreObject( std::string& id, 
                                        Ogre::SceneManager* scene){
   scene->destroyEntity(id); 
}
}
