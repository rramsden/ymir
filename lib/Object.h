#ifndef OBJECT_H
#define OBJECT_H

#include <map>
#include <string>

//Decode support
#include <ei.h>
#include <erl_interface.h>

//Used in data generic property maps
#include <boost/any.hpp>

#include "Task.h"

namespace Ymir {


    class PropList : public std::map<std::string, boost::any> {

        public:
            void setProperty( const std:: string& id, boost::any& val ){
                PropList::iterator it = this->begin();
    
                //If the key already exists, replace it
                if( (it = this->find(id)) != this->end() ){
                    this->erase(id); 
                } 
    
                this->insert(std::pair<std::string, boost::any>(id, val));
            }

            bool hasProperty(const std::string& id, boost::any* val = NULL){
                bool hasProp = false;
                PropList::iterator it = this->begin();
                
                if( (it = this->find(id)) != this->end() ){
                    hasProp = true;
                    
                    if( val ){
                        *val = it->second;
                    }
                }
    
                return hasProp;
            }

            template<typename T>
            bool hasProperty(const std::string& id, T* output){
                bool has = false;
                boost::any temp;

                if( (has = hasProperty(id, &temp)) ){
                    *output = boost::any_cast<T>(temp);
                }

                return has;
            }
    };
    
    //typedef std::map<std::string,boost::any> PropList;

    class Object {

        public:

            typedef enum {
                Invalid = 0,
                Camera,
                Light, 
                Entity,
                Window,
                Button,
                Max
            } Type;

            Object( const std::string& uuid ,
                    Ymir::Object::Type type ) :
                uuid(uuid), 
                type(type),
                ptr(NULL) {} 
            ~Object(){}

            /*void setProperty(const std::string& name, boost::any& val); 
            bool hasProperty(const std::string& name, boost::any* val);*/

            std::string getUUID(){ return uuid; }
            Ymir::Object::Type getType(){ return type; }

            virtual void create( Ymir::PropList& props ) = 0;
            virtual void update( Ymir::PropList& actions ) = 0;
            virtual void destroy() = 0;

            static int decodeString( const char* data, int* idx, std::string* output );
            static int decodeFloat( const char* data, int* idx, float* output );
            static int decodeBool( const char* data, int* idx, int* output );
            static int decodeType( const char* data, int* idx, Ymir::Object::Type* output );

        protected:

            typedef int (*propDecodeFP)( const std::string&, const char*, int*, boost::any*);
            static int decodePropListBase( propDecodeFP fp[], const char* args, int* idx, Ymir::PropList* output );

            std::string uuid;
            Ymir::Object::Type type;
            void* ptr;
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

        ObjectTask( Ymir::ObjectTask::Type task, 
                    std::string& uuid, 
                    Ymir::Object::Type objectType = Ymir::Object::Invalid,
                    Ymir::PropList props = PropList() ) : task(task), uuid(uuid), objectType(objectType), props(props){}

        virtual void create() = 0;
        virtual void update() = 0;
        virtual void destroy() = 0;

        void run(){

            switch( task ){
                case Ymir::ObjectTask::Create:
                    create();
                    break;

                case Ymir::ObjectTask::Update:
                    update();
                    break;

                case Ymir::ObjectTask::Destroy:
                    destroy();
                    break;

                default:
                    break;
            }

        }

        protected:
            Ymir::ObjectTask::Type task;

            std::string uuid;
            Ymir::Object::Type objectType;
            Ymir::PropList props;
    };

    /*template< typename T, class S >
    class PluginObject : public Ymir::Object {
    
        public:
            PluginObject( const std::string& uuid, 
                          const char* args = NULL, 
                          int* idx = NULL ) : Ymir::Object(uuid, args, idx) {}

            ~PluginObject( ){}
  
            virtual int decodeProps( const char* args, int* idx ) = 0;
            virtual int decodeProp( const std::string& prop, const char* args, int* idx ) = 0;

            void create(boost::any* State){ this->add(boost::any_cast<S>(State)); }
            void update(boost::any* State){ this->update(boost::any_cast<S>(State)); }
            void destroy(boost::any* State){ this->destroy(boost::any_cast<S>(State)); }

        protected:
            virtual T* add(S* state) = 0;
            virtual T* update(S* state) = 0;
            virtual void destroy(S* state) = 0;
      };*/
}
#endif
