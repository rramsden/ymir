#include <errno.h>

#include "OgreObject.h"

#include <OGRE/OgreCamera.h>
#include <OGRE/OgreLight.h>
#include <OGRE/OgreEntity.h>

using namespace std;
using namespace boost;
using namespace Ogre;

namespace Ymir {

    /********************** OgreCamera Definitions ************************/
    int Camera::decodeProp( const string& prop, const char* data, int* idx ){
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
    
    Ogre::Camera* Camera::create( SceneManager* scene ){
        return scene->createCamera(uuid);
    }
    
    Ogre::Camera* Camera::fetch( SceneManager* scene ){
        return scene->getCamera(uuid);
    }
    
    void Camera::set( Ogre::Camera* camera ){
        any temp;

        if( hasProperty("nearClip", &temp) ){
            camera->setNearClipDistance(any_cast<Real>(temp));
        }
    
        if( hasProperty("farClip", &temp) ){
            camera->setFarClipDistance(any_cast<Real>(temp));
        }
    }
    
    /********************** OgreLight Definitions ************************/
    int Light::decodeProp( const string& prop, const char* args, int* idx ){
        int rc = 0;
        string source = "";
    
        if( prop == "source" && !decodeString(args, idx, &source) ){
           
            //Decode light source 
            if( source == "point" ){
                props.insert(pair<string,any>(prop, Ogre::Light::LT_POINT));
            } else if( source == "spotlight" ){
                props.insert(pair<string,any>(prop, Ogre::Light::LT_SPOTLIGHT));
            } else {
                props.insert(pair<string,any>(prop, Ogre::Light::LT_DIRECTIONAL));
            }
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
    
    Ogre::Light* Light::create( Ogre::SceneManager* scene ){
        return scene->createLight(uuid);
    }
    
    Ogre::Light* Light::fetch( Ogre::SceneManager* scene ){
        return scene->getLight(uuid);
    }
    
    void Light::set( Ogre::Light* light ){
        any temp;

        if( hasProperty("source", &temp) ){
            light->setType(any_cast<Ogre::Light::LightTypes>(temp));
        }
    
        if( hasProperty("castShadows", &temp) ){
            light->setCastShadows(any_cast<bool>(temp));
        }
    }
    
    /********************** OgreEntity Definitions ************************/
    int Entity::decodeProp( const string& prop, const char* args, int* idx ){
        int rc = 0;
        string mesh = "";
    
        if( prop == "mesh" && !decodeString(args, idx, &mesh) ){
            props.insert(pair<string, any>(prop, mesh));
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
    
    Ogre::Entity* Entity::create(SceneManager* scene){
        any temp;
        string mesh = "";
    
        if( !hasProperty("mesh", &temp) ){
            return NULL;
        }
    
        mesh = any_cast<string>(temp);
    
        return scene->createEntity(uuid, mesh);
    }
    
    Ogre::Entity* Entity::fetch(SceneManager* scene){
        return scene->getEntity(uuid);
    }
    
    void Entity::set(Ogre::Entity* entity){
        any temp;

        if( hasProperty("castShadows", &temp) ){
            entity->setCastShadows(any_cast<bool>(temp));
        }
    }

    /********************** Utility Function Definitions ************************/
    int decodeReal( const char*data, int* idx, Real* output ){
        int rc = 0;
        double val = 0;
    
        if( !(rc = ei_decode_double(data, idx, &val)) ){
            *output = Real(val);
        }
    
        return rc;
    }
    
    int decodeRadian( const char* data, int* idx, Radian* output ){
        int rc = 0;
        Real val = Real(0);
    
        if( !(rc = decodeReal(data, idx, &val)) ){
            *output = Radian(val);
        }
    
        return rc;
    }
    
    int decodeColourVal( const char* data, int* idx, ColourValue* output ){
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
    
    int decodeVector3( const char* data, int* idx, Vector3* output ){
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
    
    int decodeVector4( const char* data, int* idx, Vector4* output ){
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
}
