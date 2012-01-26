#ifndef _COREFACTORY_H
#define _COREFACTORY_H

//Scene blueprints
//#include "SceneBlueprint.h"
//#include "TerrainBlueprint.h"
#include "CameraBlueprint.h"
#include "LightBlueprint.h"
#include "EntityBlueprint.h"

//GUI blueprints
#include "ButtonBlueprint.h"
#include "WindowBlueprint.h"

namespace Ymir {

    class CoreFactory {
   
        public:

            static int decode( const char* data, 
                               int* idx,
                               ObjectType& type,
                               PropList* props );

            static void create( std::string& objectID,
                                ObjectType& type,
                                PropList& props );
    
            static void update( std::string& objectID,
                                ObjectType& type,
                                PropList& actions );
    
            static void destroy( ObjectType& type, 
                                 std::string& objectID );
             
        protected:
            //Ymir::SceneBlueprint mSceneBlueprint;
            //Ymir::TerrainBlueprint mTerrainBlueprint;
            Ymir::CameraBlueprint mCameraBlueprint;
            Ymir::LightBlueprint mLightBlueprint;
            EntityBlueprint mEntityBlueprint;
            Ymir::ButtonBlueprint mButtonBlueprint;
            //Ymir::WindowBlueprint mWindowBlueprint;
           
            ObjectFactory(){};

    };

    class ObjectTask : public Task {

        public:
            typedef enum {
                Invalid = 0,
                Create,
                Update,
                Destroy,
                Max
            } Type;

        static ObjectTask create( std::string& objectID,
                                  Ymir::ObjectType,
                                  Ymir::PropList& props );

        static ObjectTask update( std::string& objectID, 
                                  Ymir::ObjectType,
                                  Ymir::PropList& props );

        static ObjectTask destroy( Ymir::ObjectType, 
                                   std::string objectID );

        void run(){

            switch( task ){
                case Ymir::ObjectTask::Create:
                    
                    ObjectFactory::create( mObjectID, mObjectType, mProps );
                    break;

                case Ymir::ObjectTask::Update:
                    ObjectFactory::update( mObjectID, mObjectType, mProp );
                    break;

                case Ymir::ObjectTask::Destroy:
                    ObjectFactory::destroy( mObjectType, mObjectID );
                    break;

                default:
                    break;
            }

        }

        protected:

        ObjectTask( Ymir::ObjectTask::Type task, 
                    std::string& objectID, 
                    Ymir::ObjectType objectType = Ymir::Object::Invalid,
                    Ymir::PropList props = PropList() ) : mType(task), 
                                                          mObjectID(objectID), 
                                                          mObjectType(objectType), 
                                                          mProps(props){}


            Ymir::ObjectTask::Type mType;

            std::string mObjectID;
            Ymir::ObjectType mObjectType;
            Ymir::PropList mProps;
    };

}

#endif
