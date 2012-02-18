#include "ObjectFactory.h"

//Scene blueprints
#include "SceneBlueprint.h"
#include "TerrainBlueprint.h"
#include "CameraBlueprint.h"
#include "LightBlueprint.h"
#include "EntityBlueprint.h"

//GUI blueprints
#include "ButtonBlueprint.h"
//#include "WindowBlueprint.h"

#include "DecodeBasic.h"
#include "Core.h"

namespace Ymir {

static SceneBlueprint mSceneBlueprint;
static TerrainBlueprint mTerrainBlueprint;
static CameraBlueprint mCameraBlueprint;
static LightBlueprint mLightBlueprint;
static EntityBlueprint mEntityBlueprint;
static ButtonBlueprint mButtonBlueprint;
//Ymir::WindowBlueprint mWindowBlueprint;

int ObjectFactory::decode( const char* data, int* idx, Object::Type type, PropList* props )
{
    int rc = 0;
   
    switch(type){

        case Object::Scene:
            rc = mSceneBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Terrain:
            rc = mTerrainBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Camera:
            rc = mCameraBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Light:
            rc = mLightBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Entity:
            rc = mEntityBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Button:
            rc = mButtonBlueprint.decodePropList(data, idx, props);
            break;

        case Object::Window:
            break;

        default:
            break;
    }

    return rc;
}

int decodeType( const char* data, int* idx, Object::Type* output ){
    int rc = 0;
    string type = "";

    if( !(rc = decodeString(data, idx, &type)) ){
       
        if( type == "scene" ){
            *output = Ymir::Object::Scene;
        } else if( type == "terrain" ){
            *output = Ymir::Object::Terrain;
        } else if( type == "camera" ){
            *output = Ymir::Object::Camera;
        } else if( type == "light" ){
            *output = Ymir::Object::Light;
        } else if( type == "entity" ){
            *output = Ymir::Object::Entity;
        } else if( type == "window" ){
            *output = Ymir::Object::Window;
        } else if( type == "button" ){
            *output = Ymir::Object::Button;   
        } else {
            *output = Ymir::Object::Invalid;
        }
    } 
    
    return rc;
}

int ObjectFactory::decodeObject( const char* data, 
                                 int* idx, 
                                 std::string* uuid,
                                 Object::Type* type,
                                 PropList* props )
{
    int arity = 0;

    if( ei_decode_tuple_header(data, idx, &arity) || 
       (arity != 3) )
    { 
        return -EINVAL;
    }

    if( decodeString(data, idx, uuid) ||
        decodeType(data, idx, type) ||
        (*type <= Object::Invalid) || 
        (*type >= Object::Max) )
    {
        return -EINVAL;
    }

    return decode( data, idx, *type, props );
}

int ObjectFactory::decodeObject( const char* data, 
                                 int* idx,
                                 Object* obj )
{
    std::string id;
    Object::Type type;
    PropList props;

    if( decodeObject(data, idx, &id, &type, &props) ){
        return -EINVAL;
    }

    *obj = Object(id, type, props);

    return 0;
}   

void ObjectFactory::create( std::string& objectID,
                            Ymir::Object::Type type,
                            Ymir::PropList& props )
{
    switch(type){

        case Object::Scene:
            mSceneBlueprint.create(objectID, props);
            break;

        case Object::Terrain:
            mTerrainBlueprint.create(objectID, props);
            break;

        case Object::Camera:
            mCameraBlueprint.create(objectID, props);
            break;

        case Object::Light:
            mLightBlueprint.create(objectID, props);
            break;

        case Object::Entity:
            mEntityBlueprint.create(objectID, props);
            break;

        case Object::Button:
            mButtonBlueprint.create(objectID, props);
            break;

        case Object::Window:
            break;

        default:
            break;
    }
}

void ObjectFactory::update( std::string& objectID,
                            Object::Type type,
                            Ymir::PropList& props )
{
    switch(type){

        case Object::Scene:
            mSceneBlueprint.update(objectID, props);
            break;

        case Object::Terrain:
            mTerrainBlueprint.update(objectID, props);
            break;

        case Object::Camera:
            mCameraBlueprint.update(objectID, props);
            break;

        case Object::Light:
            mLightBlueprint.update(objectID, props);
            break;

        case Object::Entity:
            mEntityBlueprint.update(objectID, props);
            break;

        case Object::Button:
            mButtonBlueprint.update(objectID, props);
            break;

        case Object::Window:
            break;

        default:
            break;
    }
}
    
void ObjectFactory::destroy( std::string& objectID,
                             Object::Type type,
                             PropList& props )
{
    switch(type){

        case Object::Scene:
            mSceneBlueprint.destroy(objectID, props);
            break;

        case Object::Terrain:
            mTerrainBlueprint.destroy(objectID, props);
            break;

        case Object::Camera:
            mCameraBlueprint.destroy(objectID, props);
            break;

        case Object::Light:
            mLightBlueprint.destroy(objectID, props);
            break;

        case Object::Entity:
            mEntityBlueprint.destroy(objectID, props);
            break;

        case Object::Button:
            mButtonBlueprint.destroy(objectID, props);
            break;

        case Object::Window:
            break;

        default:
            break;
    }
}

/*********** FactoryTask Definitions ************/
    FactoryTask::FactoryTask( Ymir::FactoryTask::Type task, 
                              std::string& objectID, 
                              Object::Type objectType = Ymir::Object::Invalid,
                              PropList props = PropList() ) : mType(task), 
                                                              mObjectID(objectID), 
                                                              mObjectType(objectType), 
                                                              mProps(props){}

    void FactoryTask::run(){

        switch( mType ){
            case Ymir::FactoryTask::Create:
                
                ObjectFactory::create( mObjectID, mObjectType, mProps );
                break;

            case Ymir::FactoryTask::Update:
                ObjectFactory::update( mObjectID, mObjectType, mProps );
                break;

            case Ymir::FactoryTask::Destroy:
                ObjectFactory::destroy( mObjectID, mObjectType, mProps );
                break;

            default:
                break;
        }

    }
}
