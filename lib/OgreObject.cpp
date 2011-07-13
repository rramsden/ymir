#include <errno.h>

#include "OgreObject.h"

#include <OGRE/OgreCamera.h>
#include <OGRE/OgreLight.h>
#include <OGRE/OgreEntity.h>

using namespace std;
using namespace boost;
using namespace Ogre;

/********************** OgreObject Definitions ************************/
OgreObject::OgreObject( const std::string& uuid ) :
    uuid(uuid),
    props()
{

}

OgreObject::~OgreObject(){

}

void OgreObject::setProperty(const string& name, any& val){
    PropList::iterator it = props.begin();

    //If the key already exists, replace it
    if( (it = props.find(name)) != props.end() ){
       props.erase(name); 
    } 

    props.insert(pair<string, any>(name, val));
}


bool OgreObject::hasProperty(const string& name, any* val = NULL){
    bool hasProp = false;
    PropList::iterator it = props.begin();
    
    if( (it = props.find(name)) != props.end() ){
        hasProp = true;
        
        if( val ){
            *val = it->second;
        }
    }

    return hasProp;
}

int OgreObject::decodeProps( char* data, int* idx ){
    int count = 0;

    if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
        return -EINVAL;
    }   

    //Walk the list of given properties
    for( int i = 0; i < count; i++ ){
        int arity = 0;
        int bval = false;
        string prop = "";
        Vector3 vec3;
        Vector4 vec4;

        //Every prop must be of the form {name:string, prop:varies}
        if( ei_decode_tuple_header(data, idx, &arity) ||
            (arity != 2) ||
            decodeString(data, idx, &prop) )
        {
            return -EINVAL;
        }

        if( prop == "position" && !decodeVector3(data, idx, &vec3) ){
            props.insert(pair<string, any>(prop, vec3));
        } else if( prop == "direction" && !decodeVector3(data, idx, &vec3) ){
            props.insert(pair<string, any>(prop, vec3)); 
        } else if( prop == "rotation" && !decodeVector4(data, idx, &vec4) ){
            props.insert(pair<string, any>(prop, vec4)); 
        } else if( prop == "scale" && !decodeVector3(data, idx, &vec3) ){
            props.insert(pair<string, any>(prop, vec3)); 
        } else if( prop == "isVisible" && !decodeBool(data, idx, &bval) ){
            props.insert(pair<string, any>(prop, bval));
        } else if( prop == "castShadows" && !decodeBool(data, idx, &bval) ){
            props.insert(pair<string, any>(prop, vec3));
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

/********************** OgreCamera Definitions ************************/
int OgreCamera::decodeProp( const string& prop, char* data, int* idx ){
    int rc = 0;
    Vector3 vec3;
    Real real;

    if( prop == "lookAt" && !decodeVector3(data, idx, &vec3) ){
        props.insert(pair<string,any>(prop, vec3));
    } else if( prop == "nearClip" && !decodeReal(data, idx, &real) ){
        props.insert(pair<string,any>(prop, real));
    } else if( prop == "farClip" && !decodeReal(data, idx, &real) ){
        props.insert(pair<string,any>(prop, real));
    } else {
        rc = -EINVAL;
    }

    return rc;
}

int OgreCamera::addToScene( SceneManager* scene ){
    Camera* camera = scene->createCamera(uuid);
    any temp; 

    if( hasProperty("position", &temp) ){
        camera->setPosition(any_cast<Vector3>(temp));
    } 

    if( hasProperty("direction", &temp) ){
        camera->setDirection(any_cast<Vector3>(temp));
    }

    if( hasProperty("lookAt", &temp) ){
        camera->lookAt(any_cast<Vector3>(temp));
    }

    if( hasProperty("nearClip", &temp) ){
        camera->setNearClipDistance(any_cast<Real>(temp));
    }

    if( hasProperty("farClip", &temp) ){
        camera->setFarClipDistance(any_cast<Real>(temp));
    }

    return 0;
}

/********************** OgreLight Definitions ************************/
int OgreLight::decodeProp( const string& prop, char* args, int* idx ){
    int rc = 0;
    string source = "";

    if( prop == "source" && !decodeString(args, idx, &source) ){
       
        //Decode light source 
        if( source == "point" ){
            props.insert(pair<string,any>(prop, Light::LT_POINT));
        } else if( source == "spotlight" ){
            props.insert(pair<string,any>(prop, Light::LT_SPOTLIGHT));
        } else {

            props.insert(pair<string,any>(prop, Light::LT_DIRECTIONAL));
        }
    } else {
        rc = -EINVAL;
    }

    return rc;
}

int OgreLight::addToScene( SceneManager* scene ){
    any temp;

    Light* light = scene->createLight(uuid);

    if( hasProperty("position", &temp) ){
        light->setPosition(any_cast<Vector3>(temp));
    }

    if( hasProperty("direction", &temp) ){
        light->setDirection(any_cast<Vector3>(temp));
    }

    if( hasProperty("source", &temp) ){
        light->setType(any_cast<Light::LightTypes>(temp));

        //<<HERE>> Enforce direction/position reqs based on type?
    }

    if( hasProperty("castShadows", &temp) ){
        light->setCastShadows(any_cast<bool>(temp));
    }

    return 0;
}

/********************** OgreEntity Definitions ************************/
int OgreEntity::decodeProp( const string& prop, char* args, int* idx ){
    int rc = 0;
    string mesh = "";

    if( prop == "mesh" && !decodeString(args, idx, &mesh) ){
        props.insert(pair<string, any>(prop, mesh));
    } else {
        rc = -EINVAL;
    }

    return rc;
}

int OgreEntity::addToScene( SceneManager* scene ){
    any temp;
    string mesh = "";

    //Must have at least have a mesh defined
    if( !hasProperty("mesh", &temp) ){
        return -1;
    } 
    
    mesh = any_cast<string>(temp);

    Ogre::Entity* entity = scene->createEntity(uuid, mesh);

    printf("Created enitity %s with mesh: %s\n", uuid.c_str(), mesh.c_str());

    if( hasProperty("castShadows", &temp) ){
        entity->setCastShadows(any_cast<bool>(temp));
    }

    SceneNode* node = scene->getRootSceneNode()->createChildSceneNode();

    if( hasProperty("position", &temp) ){
        node->setPosition(any_cast<Vector3>(temp));
    }

    if( hasProperty("direction", &temp) ){
        node->setDirection(any_cast<Vector3>(temp));
    }

    if( hasProperty("scale", &temp) ){
        node->setScale(any_cast<Vector3>(temp));
    }

    /*if( hasProperty("lookAt", &temp) ){
        node->lookAt(any_cast<Vector3>(temp));
    }*/

    node->attachObject(entity);

    return 0;
}

/********************** Utility Function Definitions ************************/
int decodeBool( char* data, int* idx, int* output ){
    return ei_decode_boolean(data, idx, output);
}

int decodeType( char* data, int* idx, OgreObjectType* output ){
    int rc = 0;
    string type = "";

    if( !(rc = decodeString(data, idx, &type)) ){
        
        if( type == "camera" ){
            *output = OBJECT_CAMERA;
        } else if( type == "light" ){
            *output = OBJECT_LIGHT;
        } else if( type == "entity" ){
            *output = OBJECT_ENTITY;
        } else {
            *output = OBJECT_INVALID;
        }
    }

    return rc;
}

int decodeString( char* data, int* idx, string* output ){
    int rc = 0;
    char temp [256] = {0};
    
    if( !(rc = ei_decode_string(data, idx, temp)) ){
        *output = string(temp);
    }

    return rc;
}

int decodeReal( char*data, int* idx, Real* output ){
    int rc = 0;
    double val = 0;

    if( !(rc = ei_decode_double(data, idx, &val)) ){
        *output = Real(val);
    }

    return rc;
}

int decodeColourVal( char* data, int* idx, ColourValue* output ){
    int rc = 0, len = 0;
    double r = 0, g = 0, b = 0, a = 0;

    if( ei_decode_list_header(data, idx, &len) || (len != 4) ){
        rc = -EINVAL;
        goto exit;
    } 

    for( int i = 0; i < len; i++ ){
        int arity = 0;
        string name = "";
        double val = 0;

        if( ei_decode_tuple_header(data, idx, &arity) || 
            (arity != 2) || 
            decodeString(data, idx, &name) ||
            ei_decode_double(data, idx, &val) )
        {
            rc = -EINVAL;
            goto exit;
        }

        switch(i){

            case 0:
                r = val;
                break;

            case 1:
                g = val;
                break;

            case 2:
                b = val;
                break;
            
            case 3:
                a = val;
                break;
        }

    }

    //Decode empty list
    if( ei_decode_list_header(data, idx, &len) || len ){
        rc = -EINVAL;
    } else {
        *output = ColourValue(r,g,b,a);
    }

    exit:
    return rc;
}

int decodeVector3( char* data, int* idx, Vector3* output ){
    int rc = 0;
    int len = 0;
    Real x, y, z;

    if( ei_decode_list_header(data, idx, &len) || (len != 3) ){
        rc = -EINVAL;
        goto exit;
    }

    for( int i = 0; i < len; i++ ){
        int arity = 0;
        string name = "";
        double val = 0;

        if( ei_decode_tuple_header(data, idx, &arity) || 
            (arity != 2) || 
            decodeString(data, idx, &name) ||
            ei_decode_double(data, idx, &val) )
        {
            rc = -EINVAL;
            goto exit;
        }

        switch(i){

            case 0:
                x = val;
                break;

            case 1:
                y = val;
                break;

            case 2:
                z = val;
                break;
        }
    }

    //Decode empty list
    if( ei_decode_list_header(data, idx, &len) || len ){
        rc = -EINVAL;
    } else {
        *output = Vector3(x,y,z);
    }

    exit:
    return rc;
}

int decodeVector4( char* data, int* idx, Vector4* output ){
    int rc = 0;
    int len = 0;
    Real x, y, z, w;

    if( ei_decode_list_header(data, idx, &len) || (len != 4) ){
        rc = -EINVAL;
        goto exit;
    }

    for( int i = 0; i < len; i++ ){
        int arity = 0;
        string name = "";
        double val = 0;

        if( ei_decode_tuple_header(data, idx, &arity) || 
            (arity != 2) || 
            decodeString(data, idx, &name) ||
            ei_decode_double(data, idx, &val) )
        {
            rc = -EINVAL;
            goto exit;
        }

        switch(i){

            case 0:
                x = val;
                break;

            case 1:
                y = val;
                break;

            case 2:
                z = val;
                break;

            case 3: 
                w = val;
                break;
        }
    }

    //Decode empty list
    if( ei_decode_list_header(data, idx, &len) || len ){
        rc = -EINVAL;
    } else {
        *output = Vector4(x,y,z, w);
    }

    exit:
    return rc;
}
