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
    props(),
    actions()
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

bool OgreObject::hasAction(const string& name, any* val = NULL){
    bool hasAction = false;
    PropList::iterator it = actions.begin();
    
    if( (it = actions.find(name)) != actions.end() ){
        hasAction = true;
        
        if( val ){
            *val = it->second;
        }
    }

    return hasAction;
}

int OgreObject::decodeAddProps( char* data, int* idx ){
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
        Radian radian;

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
        } else if( prop == "yaw" && !decodeRadian(data, idx, &radian) ){
            props.insert(pair<string, any>(prop, radian));
        } else if( prop == "pitch" && !decodeRadian(data, idx, &radian) ){
            props.insert(pair<string, any>(prop, radian));
        } else if( prop == "roll" && !decodeRadian(data, idx, &radian) ){
            props.insert(pair<string, any>(prop, radian));
        } else if( prop == "rotation" && !decodeVector4(data, idx, &vec4) ){
            props.insert(pair<string, any>(prop, vec4)); 
        } else if( prop == "scale" && !decodeVector3(data, idx, &vec3) ){
            props.insert(pair<string, any>(prop, vec3)); 
        } else if( prop == "isVisible" && !decodeBool(data, idx, &bval) ){
            props.insert(pair<string, any>(prop, bval));
        } else if( this->decodeAddProp(prop, data, idx) ){
            return -EINVAL;
        }    
    }

    //Decode end of list
    if( ei_decode_list_header(data, idx, &count) || count ){
        return -EINVAL;
    }

    return 0;
}

int OgreObject::decodeUpdateActions(char* args, int* idx){
    int count = 0;

    if( !args || !idx || ei_decode_list_header(args, idx, &count) ){
        return -EINVAL;
    }   

    //Walk the list of given properties
    for( int i = 0; i < count; i++ ){
        int arity = 0;
        string action = "";
        Radian radian;
        Vector3 vec3;

        //Every prop must be of the form {name:string, prop:varies}
        if( ei_decode_tuple_header(args, idx, &arity) ||
            (arity != 2) ||
            decodeString(args, idx, &action) )
        {
            return -EINVAL;
        }

        //Decode common update actions
        if( action == "move" && !decodeVector3(args, idx, &vec3) ){
            actions.insert(pair<string,any>(action, vec3));
        } else if( action == "yaw" && !decodeRadian(args, idx, &radian) ){
            actions.insert(pair<string,any>(action, radian));
        } else if( action == "pitch" && !decodeRadian(args, idx, &radian) ){
            actions.insert(pair<string,any>(action, radian));
        } else if( action == "roll" && !decodeRadian(args, idx, &radian) ){
            actions.insert(pair<string,any>(action, radian));
        }
        
        /*else if( action == "rotate" && !decodeVector3(args, idx, &vec3) ){
            actions.insert(pair<string,any>(action, vec3));
        }*/
        
    }

    //Decode end of list
    if( ei_decode_list_header(args, idx, &count) || count ){
        return -EINVAL;
    }

    return 0;
}

void OgreObject::addCommon( SceneNode* node ){
    any temp; 

    if( !node ){
        printf("no node given!!!\n");
        return;
    }

    if( hasProperty("position", &temp) ){
        node->setPosition(any_cast<Vector3>(temp));
    } 

    if( hasProperty("direction", &temp) ){
        node->setDirection(any_cast<Vector3>(temp));
    }

    if( hasProperty("yaw", &temp) ){
        node->yaw(any_cast<Radian>(temp));
    }

    if( hasProperty("pitch", &temp) ){
        node->pitch(any_cast<Radian>(temp));
    }

    if( hasProperty("roll", &temp) ){
        node->roll(any_cast<Radian>(temp));
    }

    if( hasProperty("lookAt", &temp) ){
        node->lookAt(any_cast<Vector3>(temp), Node::TS_WORLD);
    }
}


void OgreObject::updateCommon( SceneNode* node ){
    any temp;

    if( hasAction("move", &temp) ){
        node->translate( node->getOrientation() * any_cast<Vector3>(temp) );
    }

    if( hasAction("yaw", &temp) ){
        
        node->yaw(any_cast<Radian>(temp));
    }

    if( hasAction("pitch", &temp) ){
        node->pitch(any_cast<Radian>(temp));
    }

    if( hasAction("roll", &temp) ){
        node->roll(any_cast<Radian>(temp));
    }

    /*if( hasAction("rotate", &temp) ){
        Vector3 temp = any_cast<Vector3>(temp);

        node->yaw(Radian(temp.x));
        node->pitch(Radian(temp.y));
        node->roll(Radian(temp.z));
    }*/
}

/********************** OgreCamera Definitions ************************/
int OgreCamera::decodeAddProp( const string& prop, char* data, int* idx ){
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

int OgreCamera::decodeUpdateAction( const string& action, char* data, int* idx) {

    return 0;
}

int OgreCamera::add( SceneManager* scene ){
    Camera* camera = NULL; 
    SceneNode* node = NULL;
    any temp; 

    camera = scene->createCamera(uuid);
    node = scene->getRootSceneNode()->createChildSceneNode();

    node->attachObject(camera);

    printf("node pointer! %p\n", node);

    addCommon(node);

    if( hasProperty("position", &temp) ){
        camera->setPosition(any_cast<Vector3>(temp));
    } 

    if( hasProperty("direction", &temp) ){
        camera->setDirection(any_cast<Vector3>(temp));
    }

    if( hasProperty("yaw", &temp) ){
        camera->yaw(any_cast<Radian>(temp));
    }

    if( hasProperty("pitch", &temp) ){
        camera->pitch(any_cast<Radian>(temp));
    }

    if( hasProperty("roll", &temp) ){
        camera->roll(any_cast<Radian>(temp));
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

int OgreCamera::update( SceneManager* scene ){
    Camera* camera = NULL;

    try {
        camera = scene->getCamera(uuid);
    } catch( ... ){
        return -EINVAL;
    }

    updateCommon(camera->getParentSceneNode()); 
    
    return 0;
}

/********************** OgreLight Definitions ************************/
int OgreLight::decodeAddProp( const string& prop, char* args, int* idx ){
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


int OgreLight::decodeUpdateAction( const string& action, char* data, int* idx) {

    return 0;
}

int OgreLight::add( SceneManager* scene ){
    Light* light = NULL;
    SceneNode* node = NULL;
    any temp;

    printf("Huh?\n");

    light = scene->createLight(uuid);
    node = scene->getRootSceneNode()->createChildSceneNode();

    node->attachObject(light);
   
    printf("Before addCommon, node = %p\n", light->getParentSceneNode());

    addCommon(light->getParentSceneNode());

    printf("After addCommon\n");

    if( hasProperty("source", &temp) ){
        light->setType(any_cast<Light::LightTypes>(temp));
    }

    if( hasProperty("castShadows", &temp) ){
        light->setCastShadows(any_cast<bool>(temp));
    }

    return 0;
}

int OgreLight::update( SceneManager* scene ){
    Light* light = NULL;
    any temp;
    
    try {
        light = scene->getLight(uuid);
    } catch( ... ){
        return -EINVAL;
    }

    updateCommon(light->getParentSceneNode());

    return 0;
}

/********************** OgreEntity Definitions ************************/
int OgreEntity::decodeAddProp( const string& prop, char* args, int* idx ){
    int rc = 0;
    string mesh = "";

    if( prop == "mesh" && !decodeString(args, idx, &mesh) ){
        props.insert(pair<string, any>(prop, mesh));
    } else {
        rc = -EINVAL;
    }

    return rc;
}

int OgreEntity::decodeUpdateAction( const string& action, char* data, int* idx) {

    return 0;
}

int OgreEntity::add( SceneManager* scene ){
    Ogre::Entity* entity = NULL;
    SceneNode* node = NULL;
    any temp;
    string mesh = "";

    if( !hasProperty("mesh", &temp) ){
        return -EINVAL;
    }

    mesh = any_cast<string>(temp);

    entity = scene->createEntity(uuid, mesh);
    node = scene->getRootSceneNode()->createChildSceneNode();
    node->attachObject(entity);

    addCommon(node);

    printf("Created enitity %s with mesh: %s\n", uuid.c_str(), mesh.c_str());

    if( hasProperty("castShadows", &temp) ){
        entity->setCastShadows(any_cast<bool>(temp));
    }

    return 0;
}

int OgreEntity::update( SceneManager* scene ){
    Ogre::Entity* entity = NULL;
    any temp;
    
    try {
        entity = scene->getEntity(uuid);
    } catch (...) {
        return -EINVAL;
    }

    updateCommon(entity->getParentSceneNode());

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

int decodeRadian( char* data, int* idx, Radian* output ){
    int rc = 0;
    Real val = Real(0);

    if( !(rc = decodeReal(data, idx, &val)) ){
        *output = Radian(val);
    }

    return rc;
}

int decodeColourVal( char* data, int* idx, ColourValue* output ){
    int rc = 0, arity = 0;
    double r = 0, g = 0, b = 0, a = 0;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 4) ){
        rc = -EINVAL;
        goto exit;
    } 

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) )
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

    *output = ColourValue(r,g,b,a);

    exit:
    return rc;
}

int decodeVector3( char* data, int* idx, Vector3* output ){
    int rc = 0;
    int arity = 0;
    Real x, y, z;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 3) ){
        rc = -EINVAL;
        goto exit;
    }

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) )
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

    *output = Vector3(x,y,z);
    
    exit:
    return rc;
}

int decodeVector4( char* data, int* idx, Vector4* output ){
    int rc = 0;
    int arity = 0;
    Real x, y, z, w;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 4) ){
        rc = -EINVAL;
        goto exit;
    }

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) )
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

    *output = Vector4(x,y,z, w);

    exit:
    return rc;
}
