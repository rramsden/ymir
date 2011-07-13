#ifndef OGREOBJECT_H
#define OGREOBJECT_H

#include <map>
#include <string>

//Ogre manager integration
#include <OGRE/OgreSceneManager.h>
#include <OGRE/OgreSceneNode.h>

//Decode support
#include <ei.h>
#include <erl_interface.h>

//Used in data generic property maps
#include <boost/any.hpp>

typedef std::map<std::string,boost::any> PropList;

typedef enum {
    OBJECT_INVALID = 0,
    OBJECT_CAMERA,
    OBJECT_LIGHT, 
    OBJECT_ENTITY,
    OBJECT_MAX
} OgreObjectType;

//Exported utility functions (Didn't really have a better place for them)
int decodeBool( char* data, int* idx, int* output );
int decodeType( char* data, int* idx, OgreObjectType* output );
int decodeString( char* data, int* idx, std::string* output );
int decodeReal( char*data, int* idx, Ogre::Real* output );
int decodeVector3( char* data, int* idx, Ogre::Vector3* output );
int decodeVector4( char* data, int* idx, Ogre::Vector4* output );

class OgreObject {

    public:
        OgreObject( const std::string& uuid );
        ~OgreObject();

        void setProperty(const std::string& name, boost::any& val);
        bool hasProperty(const std::string& name, boost::any* val);

        int decodeProps( char* args, int* idx );
        
        virtual int decodeProp( const std::string& prop, char* args, int* idx ) = 0;
        virtual int addToScene( Ogre::SceneManager* scene ) = 0;

    protected:
        std::string uuid;
        PropList props;
};

class OgreCamera : public OgreObject {

    public:
        OgreCamera( const std::string& uuid ) : OgreObject(uuid) {};
        ~OgreCamera() {};

        int decodeProp( const std::string& prop, char* args, int* idx );
        int addToScene( Ogre::SceneManager* scene );
};

class OgreLight : public OgreObject {

    public:
        OgreLight( const std::string& uuid ) : OgreObject(uuid) {};
        ~OgreLight() {};

        int decodeProp( const std::string& prop, char* args, int* idx );
        int addToScene( Ogre::SceneManager* scene );
};

class OgreEntity : public OgreObject {
    public:
        OgreEntity( const std:: string& uuid ) : OgreObject(uuid) {};
        ~OgreEntity() {};

        int decodeProp( const std::string& prop, char* args, int* idx );
        int addToScene( Ogre::SceneManager* scene );
};

#endif
