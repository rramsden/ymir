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
int decodeRadian( char* data, int* idx, Ogre::Radian* output );
int decodeVector3( char* data, int* idx, Ogre::Vector3* output );
int decodeVector4( char* data, int* idx, Ogre::Vector4* output );

class OgreObject {

    public:
        OgreObject( const std::string& uuid );
        ~OgreObject();

        void setProperty(const std::string& name, boost::any& val);

        bool hasProperty(const std::string& name, boost::any* val);
        bool hasAction(const std::string& name, boost::any* val);

        int decodeAddProps( char* args, int* idx );
        int decodeUpdateActions( char* args, int* idx ); 

        virtual int decodeAddProp( const std::string& prop, char* args, int* idx ) = 0;
        virtual int decodeUpdateAction( const std::string& prop, char* args, int* idx) = 0;

        virtual int update( Ogre::SceneManager* scene ) = 0;
        virtual int add( Ogre::SceneManager* scene ) = 0;

    protected:

        void addCommon( Ogre::SceneNode* parent );
        void updateCommon( Ogre::SceneNode* parent );

        std::string uuid;
        PropList props;
        PropList actions;
};

class OgreCamera : public OgreObject {

    public:
        OgreCamera( const std::string& uuid ) : OgreObject(uuid) {};
        ~OgreCamera() {};

        int decodeAddProp( const std::string& prop, char* args, int* idx );
        int decodeUpdateAction( const std::string& action, char* args, int* idx );

        int add( Ogre::SceneManager* scene );
        int update( Ogre::SceneManager* scene );
};

class OgreLight : public OgreObject {

    public:
        OgreLight( const std::string& uuid ) : OgreObject(uuid) {};
        ~OgreLight() {};

        int decodeAddProp( const std::string& prop, char* args, int* idx );
        int decodeUpdateAction( const std::string& action, char* args, int* idx );

        int add( Ogre::SceneManager* scene );
        int update( Ogre::SceneManager* scene );
};

class OgreEntity : public OgreObject {
    public:
        OgreEntity( const std:: string& uuid ) : OgreObject(uuid) {};
        ~OgreEntity() {};

        int decodeAddProp( const std::string& prop, char* args, int* idx );
        int decodeUpdateAction( const std::string& action, char* args, int* idx );

        int add( Ogre::SceneManager* scene );
        int update( Ogre::SceneManager* scene );
};

#endif
