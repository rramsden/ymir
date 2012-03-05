#include "StaticEntityBlueprint.h"

using namespace Ogre;

namespace Ymir {
StaticEntityBlueprint::StaticEntityBlueprint() : NodeBlueprint<Ogre::Entity>(){

    mBlueprint["mesh"] = BPFP(&decodeString, NULL);
    mBlueprint["shape"] = BPFP(&decodePrefabType, NULL);
    mBlueprint["material"] = BPFP(&decodeString, (setFP)&setMaterial);
}

int StaticEntityBlueprint::decodePrefabType( const char* data, 
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

void StaticEntityBlueprint::setMaterial( NodeInfo<Ogre::Entity>* t, boost::any& mat ){
    t->mObject->setMaterialName(boost::any_cast<std::string>(mat));
}

Entity* createPrimitive(std::string& id, PropList& props, SceneManager* scene){
    SceneManager::PrefabType type;

    if( props.hasProperty<SceneManager::PrefabType>("shape", &type) ){
        return scene->createEntity(id, type); 
    } else {
        return NULL;
    }
}

Entity* createMesh(std::string& id, PropList& props, SceneManager* scene){
    std::string mesh = "";

    if( props.hasProperty<std::string>("mesh", &mesh) ){
        return scene->createEntity(id, mesh);
    } else {
        return NULL;
    }

}

Ogre::Entity* StaticEntityBlueprint::createOgreObject(std::string& id,
                                                Ymir::PropList& props,
                                                Ogre::SceneManager* scene)
{
    Entity* out = NULL;

    //Mesh must be defined otherwise throw exception
    if( !(out = createPrimitive(id, props, scene)) ){
        out = createMesh(id, props, scene);
    } 
    
    return out;
}

btCollisionShape* createPrimitiveShape( BtOgre::StaticMeshToShapeConverter& conv, 
                                        PropList& props )
{
    btCollisionShape* out = NULL;
    SceneManager::PrefabType type;

    if( !props.hasProperty<SceneManager::PrefabType>("shape", &type) ){
        return NULL;
    }

    switch(type){
        case Ogre::SceneManager::PT_PLANE:
        case Ogre::SceneManager::PT_CUBE:
            out = conv.createBox();
            break;

        case Ogre::SceneManager::PT_SPHERE:
            out = conv.createSphere(); 
            break;
    }

    return out;
}

btCollisionShape* StaticEntityBlueprint::createPhysicsObject( Entity* ent,
                                                              PropList& props )
{
    btCollisionShape* out = NULL;
    BtOgre::StaticMeshToShapeConverter converter(ent);

    if( !(out = createPrimitiveShape(converter, props)) ){
        
        Core::getSingletonPtr()->logNormal(
                "Creating box shape around static,non-primitive entity!");
        out = converter.createBox();
    }

    return out;
}

Ogre::Entity* StaticEntityBlueprint::findOgreObject( std::string& id, 
                                               Ogre::SceneManager* scene){
    return scene->getEntity(id);
}

void StaticEntityBlueprint::destroyOgreObject( std::string& id, 
                                        Ogre::SceneManager* scene){
   scene->destroyEntity(id); 
}
}
