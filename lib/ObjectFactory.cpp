#include "ObjectFactory.h"

#include "Core.h"

namespace Ymir {

void ObjectFactory::create( std::string& objectID,
                            Ymir::ObjectType type,
                            Ymir::PropList& props )
{
    Ymir::Core* core = Ymir::Core::getInstancePtr();
    Ymir::Scene* scene = NULL;

    switch(type){

        case Ymir::Scene:
            break;

        case Ymir::Terrain:
            break;

        case Ymir::Camera:
            mCameraBlueprint.create(objectID, props);
            break;

        case Ymir::Light:
            mLightBlueprint.create(objectID, props);
            break;

        case Ymir::Entity:
            mEntityBlueprint.create(objectID, props);
            break;

        case Ymir::Button:
            mButtonBlueprint.create(objectID, props);
            break;

        case Ymir::Window:
            break;

        default:
            break;
    }
}

void ObjectFactory::update( std::string& objectID,
                            ObjectType& type,
                            Ymir::PropList& props )
{
    switch(type){

        case Ymir::Scene:
            break;

        case Ymir::Terrain:
            break;

        case Ymir::Camera:
            mCameraBlueprint.update(objectID, props)
            break;

        case Ymir::Light:
            mLightBlueprint.update(objectID, props);
            break;

        case Ymir::Entity:
            mEntityBlueprint.update(objectID, props);
            break;

        case Ymir::Button:
            mButtonBlueprint.update(objectID, props);
            break;

        case Ymir::Window:
            break;

        default:
            break;
    }

}
    
void ObjectFactory::destroy( std::string& objectID,
                             ObjectType& type )
{
    switch(type){

        case Ymir::Scene:
            break;

        case Ymir::Terrain:
            break;

        case Ymir::Camera:
            mCameraBlueprint.destroy(objectID)
            break;

        case Ymir::Light:
            mLightBlueprint.destroy(objectID);
            break;

        case Ymir::Entity:
            mEntityBlueprint.destroy(objectID);
            break;

        case Ymir::Button:
            mButtonBlueprint.destroy(objectID);
            break;

        case Ymir::Window:
            break;

        default:
            break;
    }


}
}
