#ifndef OGREOBJECT_H
#define OGREOBJECT_H

//Ogre manager integration
#include <OGRE/OgreSceneManager.h>
#include <OGRE/OgreSceneNode.h>

#include "YmirObject.h"

typedef std::map<std::string,boost::any> PropList;
 
namespace Ymir {

    //Exported utility functions (Didn't really have a better place for them)
    int decodeReal( const char*data, int* idx, Ogre::Real* output );
    int decodeRadian( const char* data, int* idx, Ogre::Radian* output );
    int decodeVector3( const char* data, int* idx, Ogre::Vector3* output );
    int decodeVector4( const char* data, int* idx, Ogre::Vector4* output );

    template<typename T>
    class OgreObject : public YmirObject {
    
        public:
            OgreObject( const std::string& uuid,
                        const char* data,
                        int* idx);
            ~OgreObject() {};
   
            int decodeProps( const char* data, int* idx );

            int add( Ogre::SceneManager* scene );
            int update( Ogre::SceneManager* scene );
   
        protected:
          
            virtual int decodeProp( const std::string& prop, const char* args, int* idx ) = 0;

            //Sets properties belonging to generic scene node 
            void set( Ogre::SceneNode* node );
    
            //Sets object specific properties
            virtual void set( T* object ) = 0;

            virtual T* create( Ogre::SceneManager* scene ) = 0;
            virtual T* fetch( Ogre::SceneManager* scene ) = 0;
    };

/********************** OgreObject Definitions ************************/
    template<typename T>
    OgreObject<T>::OgreObject(const std::string& uuid, 
                              const char* data, 
                              int* idx) : YmirObject(uuid)
    {
        this->decodeProps(data, idx);
    }
    
    template<typename T>
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
                props.insert(std::pair<std::string, boost::any>(prop, vec3));
            } else if( prop == "move" && !decodeVector3(data, idx, &vec3) ){
                props.insert(std::pair<std::string,boost::any>(prop, vec3));
            } else if( prop == "direction" && !decodeVector3(data, idx, &vec3) ){
                props.insert(std::pair<std::string, boost::any>(prop, vec3)); 
            } else if( prop == "yaw" && !decodeRadian(data, idx, &radian) ){
                props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "pitch" && !decodeRadian(data, idx, &radian) ){
                props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "roll" && !decodeRadian(data, idx, &radian) ){
                props.insert(std::pair<std::string, boost::any>(prop, radian));
            } else if( prop == "rotation" && !decodeVector4(data, idx, &vec4) ){
                props.insert(std::pair<std::string, boost::any>(prop, vec4)); 
            } else if( prop == "scale" && !decodeVector3(data, idx, &vec3) ){
                props.insert(std::pair<std::string, boost::any>(prop, vec3)); 
            } else if( prop == "isVisible" && !decodeBool(data, idx, &bval) ){
                props.insert(std::pair<std::string, boost::any>(prop, bval));
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
    int OgreObject<T>::add( Ogre::SceneManager* scene ){
    
        T* object = this->create(scene);
        if( !object ){
            return -1;
        }

        //Attach the newly created object to a node
        Ogre::SceneNode* node = scene->getRootSceneNode()->createChildSceneNode();
    
        node->attachObject(object); 
    
        //Set node generic properties
        set(node);
    
        //Call object specific set function
        set(object);
    
        return 0;
    }
    
    template<typename T>
    int OgreObject<T>::update( Ogre::SceneManager* scene ){
        T* object = NULL; 
        try {
            object = this->fetch(scene);
        } catch( ... ){
            return -EINVAL;
        }
    
        //Update node properties
        set(object->getParentSceneNode());
    
        //Update object specific properties
        set(object);
    
        return 0;
    }
    
    template<typename T>
    void OgreObject<T>::set( Ogre::SceneNode* node ){
        boost::any temp;

        if( hasProperty("position", &temp) ){
            node->setPosition(boost::any_cast<Ogre::Vector3>(temp));
        } 
    
        if( hasProperty("move", &temp) ){
            node->translate( node->getOrientation() * boost::any_cast<Ogre::Vector3>(temp) );
        }
    
        if( hasProperty("direction", &temp) ){
            node->setDirection(boost::any_cast<Ogre::Vector3>(temp));
        }
    
        if( hasProperty("yaw", &temp) ){
            node->yaw(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( hasProperty("pitch", &temp) ){
            node->pitch(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( hasProperty("roll", &temp) ){
            node->roll(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( hasProperty("lookAt", &temp) ){
            node->lookAt(boost::any_cast<Ogre::Vector3>(temp), Ogre::Node::TS_WORLD);
        }
    }

    class Camera : public OgreObject<Ogre::Camera> {
    
        public:
            Camera( const std::string& uuid,
                    const char* args,
                    int* idx ) : OgreObject<Ogre::Camera>(uuid, args, idx) {};
            ~Camera() {};
    
            int decodeProp( const std::string& prop, const char* args, int* idx );
    
        protected:

            void set( Ogre::Camera* camera );

            Ogre::Camera* create( Ogre::SceneManager* scene );
            Ogre::Camera* fetch( Ogre::SceneManager* scene );
    };
    
    class Light : public OgreObject<Ogre::Light> {
    
        public:
            Light( const std::string& uuid,
                   const char* args,
                   int* idx ) : OgreObject<Ogre::Light>(uuid, args, idx) {};
            ~Light() {};
    
            int decodeProp( const std::string& prop, const char* args, int* idx );
    
        protected:
            void set( Ogre::Light* light );

            Ogre::Light* create( Ogre::SceneManager* scene );
            Ogre::Light* fetch( Ogre::SceneManager* scene );
    };
    
    class Entity : public OgreObject<Ogre::Entity> {
        public:
            Entity( const std:: string& uuid,
                    const char* args,
                    int* idx ) : OgreObject<Ogre::Entity>(uuid, args, idx) {};
            ~Entity() {};
    
            int decodeProp( const std::string& prop, const char* args, int* idx );
    
        protected:
            void set( Ogre::Entity* Entity );

            Ogre::Entity* create( Ogre::SceneManager* scene );
            Ogre::Entity* fetch( Ogre::SceneManager* scene );
    };
}

#endif
