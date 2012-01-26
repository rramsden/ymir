#ifndef _NODEOBJECT_H
#define _NODEOBJECT_H

//Ogre manager integration
#include <OgreSceneManager.h>
#include <OgreSceneNode.h>

#include <OgreCamera.h>
#include <OgreLight.h>
#include <OgreEntity.h>
#include <Terrain/OgreTerrain.h>
#include <Terrain/OgreTerrainGroup.h>

#include "OgreObject.h"

typedef std::map<std::string,boost::any> PropList;
 
namespace Ymir {

    class NodeObject : public Ymir::OgreObject {
   
        public:
            NodeObject( const std::string& uuid,
                        Ymir::Object::Type type ) : Ymir::OgreObject(uuid, type) {} 

            ~NodeObject(){}
   
            void create( Ymir::PropList& props );
            void update( Ymir::PropList& actions );
            void destroy();

            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );
        protected:
          
            //Sets properties belonging to generic scene node 
            void setNodeCommon( Ogre::SceneNode* node, 
                                Ymir::PropList& props );
    
            //Sets object specific properties
            virtual void set( Ogre::SceneNode* node, 
                              Ogre::MovableObject* object, 
                              Ymir::PropList& props ) = 0;

            virtual Ogre::MovableObject* create( Ogre::SceneManager* scene, Ymir::PropList& props ) = 0;
            virtual Ogre::MovableObject* fetch( Ogre::SceneManager* scene ) = 0;
            virtual void destroy( Ogre::SceneManager* scene ) = 0;
    };


    class Terrain : public Ymir::OgreObject {

        public:
            Terrain( const std::string& uuid ) : Ymir::Object(uuid, Ymir::Object::Terrain){}
            ~Terrain() {}

            void create( Ymir::PropList& props );
            void update( Ymir::PropList& actions );
            void destroy();

            static int decodePropList( const char* args, int* idx, Ymir::PropList* output );

            static int decodeAlign( const char* data,
                                    int* idx, 
                                    Ogre::Terrain::Alignment* out);

            /*static int decodeGridLoc( const char* data,
                                      int * idx,
                                      std::pair<long, long>* out );*/

        protected: 

            static int decodeProperty( const std::string& prop,
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );

            void set( Ymir::PropList& props );
    };

    class Camera : public NodeObject {
    
        public:
            Camera( const std::string& uuid ) : NodeObject(uuid, Ymir::Object::Camera){}
            ~Camera() {}
   
            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );
        protected:

            void set( Ogre::SceneNode* node, 
                      Ogre::MovableObject* object, 
                      Ymir::PropList& props );

            Ogre::MovableObject* create( Ogre::SceneManager* scene, Ymir::PropList& props );
            Ogre::MovableObject* fetch( Ogre::SceneManager* scene );
            void destroy( Ogre::SceneManager* );
    };
    
    class Light : public NodeObject {
    
        public:
            Light( const std::string& uuid ) : NodeObject(uuid, Ymir::Object::Light) {}
            ~Light() {}
   
            static int decodePropList( const char* args, int* idx, Ymir::PropList* output );

            protected:
            
            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );
    
            void set( Ogre::SceneNode* node, 
                      Ogre::MovableObject* object, 
                      Ymir::PropList& props );

            Ogre::MovableObject* create( Ogre::SceneManager* scene, Ymir::PropList& props );
            Ogre::MovableObject* fetch( Ogre::SceneManager* scene );
            void destroy( Ogre::SceneManager* );
    };
    
    class Entity : public NodeObject {
        public:
            Entity( const std:: string& uuid ) : NodeObject(uuid, Ymir::Object::Entity) {}
            ~Entity() {}
    
            static int decodePropList( const char* args, int* idx, Ymir::PropList* output );

        protected:
            
            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );

            void set( Ogre::SceneNode* node, 
                      Ogre::MovableObject* object, 
                      Ymir::PropList& props );

            Ogre::MovableObject* create( Ogre::SceneManager* scene, Ymir::PropList& props );
            Ogre::MovableObject* fetch( Ogre::SceneManager* scene );
            void destroy( Ogre::SceneManager* );
    };

    /*template<typename T>
    int OgreObject<T>::decodeProps( const char* data, int* idx ){
        int count = 0;
    
        if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }   
    
        //Walk the list of given properties
        for( int i = 0; i < count; i++ ){
            int arity = 0;
            int bval = false;
            std::string prop = "";
            Ogre::Vector3 vec3;
            Ogre::Vector4 vec4;
            Ogre::Radian radian;
    
            //Every prop must be of the form {name:string, prop:varies}
            if( ei_decode_tuple_header(data, idx, &arity) ||
                (arity != 2) ||
                decodeString(data, idx, &prop) )
            {
                return -EINVAL;
            }
    
            if( prop == "position" && !decodeVector3(data, idx, &vec3) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, vec3));
            } else if( prop == "move" && !decodeVector3(data, idx, &vec3) ){
                this->props.insert(std::pair<std::string,boost::any>(prop, vec3));
            } else if( prop == "direction" && !decodeVector3(data, idx, &vec3) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, vec3)); 
            } else if( prop == "yaw" && !decodeRadian(data, idx, &radian) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "pitch" && !decodeRadian(data, idx, &radian) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "roll" && !decodeRadian(data, idx, &radian) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "rotation" && !decodeVector4(data, idx, &vec4) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, vec4)); 
            } else if( prop == "scale" && !decodeVector3(data, idx, &vec3) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, vec3)); 
            } else if( prop == "isVisible" && !decodeBool(data, idx, &bval) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, bval));
            } else if( this->decodeProp(prop, data, idx) ){
                return -EINVAL;
            }    
        }
    
        //Decode end of list
        if( ei_decode_list_header(data, idx, &count) || count ){
            return -EINVAL;
        }
    
        return 0;
    }
   
    template<typename T>
    T* OgreObject<T>::add( Ogre::PCZSceneManager* scene ){

        T* object = this->create(scene);
        if( !object ){
            return NULL;
        }

        //Attach the newly created object to a node
        Ogre::SceneNode* node = scene->getRootSceneNode()->createChildSceneNode();
    
        node->attachObject(object); 
    
        //Set node generic properties
        set(node);
    
        //Call object specific set function
        set(object);
    
        return object;
    }
    
    template<typename T>
    T* OgreObject<T>::update( Ogre::PCZSceneManager* scene ){
        T* object = NULL; 
        try {
            object = this->fetch(scene);
        } catch( ... ){
            return NULL;
        }
    
        //Update node properties
        set(object->getParentSceneNode());
    
        //Update object specific properties
        set(object);
    
        return object;
    }
   
    template<typename T>
    void OgreObject<T>::destroy( Ogre::PCZSceneManager* scene ){
        T* object = this->fetch(scene);

        this->destroy(object, scene);
    }

    template<typename T>
    void OgreObject<T>::set( Ogre::SceneNode* node ){

    }*/
}

#endif
