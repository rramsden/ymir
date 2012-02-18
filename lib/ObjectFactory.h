#ifndef _OBJECTFACTORY_H
#define _OBJECTFACTORY_H

#include "Task.h"
#include "PropList.h"

namespace Ymir {

    class Object {    

        public:
        typedef enum {
            Invalid = 0,
            Scene,
            Terrain,
            Camera,
            Light, 
            Entity,
            Window,
            Button,
            Max
        } Type;

        Object() : id(""), type(Invalid), props() {}

        Object( std::string id, 
                Type type,
                PropList props ) : id(id), type(type), props(props){}
        ~Object(){}

        std::string id;
        Type type;
        PropList props;
    };

    class ObjectFactory {
   
        public:

            static int decode( const char* data, 
                               int* idx,
                               Object::Type type,
                               PropList* props );

            static int decodeObject( const char* data, 
                                     int* idx,
                                     std::string* id,
                                     Object::Type* type,
                                     PropList* props );

            static int decodeObject( const char* data,
                                     int* idx, 
                                     Object* obj );

            static void create( std::string& objectID,
                                Object::Type type,
                                PropList& props );
    
            static void update( std::string& objectID,
                                Object::Type type,
                                PropList& props );
    
            static void destroy( std::string& objectID,
                                 Object::Type type, 
                                 PropList& props );
             
        protected:

            ObjectFactory(){};

    };

    class FactoryTask : public Task {

        public:
            typedef enum {
                INVALID = 0,
                Create,
                Update,
                Destroy,
                Max
            } Type;

        FactoryTask( Ymir::FactoryTask::Type task, 
                     std::string& objectID, 
                     Object::Type objectType,
                     PropList props);
        
        void run();

        protected:

            Ymir::FactoryTask::Type mType;
            std::string mObjectID;
            Ymir::Object::Type mObjectType;
            Ymir::PropList mProps;
    };
}

#endif
