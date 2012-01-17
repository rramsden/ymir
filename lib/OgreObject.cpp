#include <errno.h>

//#include "Core.h"
#include "OgreObject.h"

#include "Core.h"

using namespace std;
using namespace boost;
using namespace Ogre;

namespace Ymir {

    int OgreObject::decodeProperty( const string& prop,
                                    const char* data, 
                                    int* idx,
                                    boost::any* output )
    {
        int bval = 0;
        Ogre::Vector3 vec3;
        Ogre::Vector4 vec4;
        Ogre::Radian radian;
            
        if( prop == "position" && !decodeVector3(data, idx, &vec3) ){
            *output = vec3;
        } else if( prop == "move" && !decodeVector3(data, idx, &vec3) ){
            *output = vec3;
        } else if( prop == "direction" && !decodeVector3(data, idx, &vec3) ){
            *output = vec3; 
        } else if( prop == "yaw" && !decodeRadian(data, idx, &radian) ){
            *output = radian;
        } else if( prop == "pitch" && !decodeRadian(data, idx, &radian) ){
            *output = radian;
        } else if( prop == "roll" && !decodeRadian(data, idx, &radian) ){
            *output = radian;
        } else if( prop == "rotation" && !decodeVector4(data, idx, &vec4) ){
            *output = vec4;
        } else if( prop == "scale" && !decodeVector3(data, idx, &vec3) ){
            *output = vec3; 
        } else if( prop == "isVisible" && !decodeBool(data, idx, &bval) ){
            *output = bval;
        } else {
            return -EINVAL;
        }    
    
        return 0;
    }

    void OgreObject::setNodeCommon( Ogre::SceneNode* node, 
                                    Ymir::PropList& props )
    {
       //Set node generic properties
        boost::any temp;
        if( props.hasProperty("position", &temp) ){
            node->setPosition(boost::any_cast<Ogre::Vector3>(temp));
        } 
    
        if( props.hasProperty("move", &temp) ){
            node->translate( node->getOrientation() * boost::any_cast<Ogre::Vector3>(temp) );
        }
    
        if( props.hasProperty("direction", &temp) ){
            node->setDirection(boost::any_cast<Ogre::Vector3>(temp));
        }
    
        if( props.hasProperty("yaw", &temp) ){
            node->yaw(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("pitch", &temp) ){
            node->pitch(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("roll", &temp) ){
            node->roll(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("lookAt", &temp) ){
            node->lookAt(boost::any_cast<Ogre::Vector3>(temp), Ogre::Node::TS_WORLD);
        }
    }

    void OgreObject::create( Ymir::PropList& props ){
        Ogre::SceneManager* scene = Ymir::Core::getSingletonPtr()->getScene();


        //Create Specific object
        Ogre::MovableObject* object = this->create(scene, props);
        if( !object ){
            //<<HERE>> Throw exception
        }

        ptr = object;

        //Create a scene node
        Ogre::SceneNode* node = scene->getRootSceneNode()->createChildSceneNode(); 

        //Attach object to node
        node->attachObject( object );

        //Inform the object to set its properties
        this->set(node, object, props);
    }

    void OgreObject::update( Ymir::PropList& props ){
        Ogre::SceneManager* scene = Ymir::Core::getSingletonPtr()->getScene();
        Ogre::MovableObject* object = this->fetch(scene);

        //Update node properties
        Ogre::SceneNode* node = object->getParentSceneNode();
        if( !node ){
            //<<HERE>> Throw exception
        }

        set(node, object, props);
    }

    void OgreObject::destroy(){
        Ogre::SceneManager* scene = Ymir::Core::getSingletonPtr()->getScene(); 

        this->destroy(scene);
    }


    int OgreObject::decodeReal( const char*data, int* idx, Real* output ){
        int rc = 0;
        double val = 0;
    
        if( !(rc = ei_decode_double(data, idx, &val)) ){
            *output = Real(val);
        }
    
        return rc;
    }
    
    int OgreObject::decodeRadian( const char* data, int* idx, Radian* output ){
        int rc = 0;
        Real val = Real(0);
    
        if( !(rc = decodeReal(data, idx, &val)) ){
            *output = Radian(val);
        }
    
        return rc;
    }
    
    int OgreObject::decodeColourVal( const char* data, int* idx, ColourValue* output ){
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
    
    int OgreObject::decodeVector3( const char* data, int* idx, Vector3* output ){
        int rc = 0;
        int arity = 0;
        Real x = Real(0), y = Real(0), z = Real(0);
    
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
                    x = Real(val);
                    break;
    
                case 1:
                    y = Real(val);
                    break;
    
                case 2:
                    z = Real(val);
                    break;
            }
        }
    
        *output = Vector3(x,y,z);
        
        exit:
        return rc;
    }
    
    int OgreObject::decodeVector4( const char* data, int* idx, Vector4* output ){
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

/********************** Camera Definitions ************************/
    int Camera::decodePropList( const char* data, 
                                int* idx, 
                                Ymir::PropList* output )
    {
        propDecodeFP fps[] = { Ymir::OgreObject::decodeProperty,
                               Ymir::Camera::decodeProperty,
                               NULL };

        return Ymir::Object::decodePropListBase( fps, data, idx, output ); 
    }
    
    int Camera::decodeProperty( const string& prop, 
                                const char* data, 
                                int* idx,
                                boost::any* output ){
        int rc = 0;
        int fix = false;
        Vector3 vec3;
        Real real;
    
        if( prop == "lookAt" && !decodeVector3(data, idx, &vec3) ){
            *output = vec3;
        } else if( prop == "nearClip" && !decodeReal(data, idx, &real) ){
            *output = real;
        } else if( prop == "farClip" && !decodeReal(data, idx, &real) ){
            *output = real;
        } else if( prop == "fixYaw" && !decodeBool(data, idx, &fix) ){
            *output = (bool)fix;
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
    
    Ogre::MovableObject* Camera::create( Ogre::SceneManager* scene,
                                         Ymir::PropList& props )
    {
    
        printf("Ogre camera create!\n");
        return scene->createCamera(uuid);
    }
    
    Ogre::MovableObject* Camera::fetch( Ogre::SceneManager* scene ){
        return scene->getCamera(uuid);
    }
   
    void Camera::destroy( Ogre::SceneManager* scene ){
        scene->destroyCamera( uuid );
    }

    void Camera::set( Ogre::SceneNode* node,
                      Ogre::MovableObject* object, 
                      Ymir::PropList& props )
    {
        Ogre::Camera* camera = static_cast<Ogre::Camera*>(object);
        any temp;

        //Camera overrides SceneNode properties
        if( props.hasProperty("position", &temp) ){
            camera->setPosition(boost::any_cast<Ogre::Vector3>(temp));
        } 
    
        if( props.hasProperty("move", &temp) ){
            camera->moveRelative(boost::any_cast<Ogre::Vector3>(temp));
            //node->translate( node->getOrientation() * boost::any_cast<Ogre::Vector3>(temp) );
        }
    
        if( props.hasProperty("direction", &temp) ){
            camera->setDirection(boost::any_cast<Ogre::Vector3>(temp));
        }
    
        if( props.hasProperty("yaw", &temp) ){
            camera->yaw(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("pitch", &temp) ){
            camera->pitch(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("roll", &temp) ){
            camera->roll(boost::any_cast<Ogre::Radian>(temp));
        }
    
        if( props.hasProperty("lookAt", &temp) ){
            camera->lookAt(boost::any_cast<Ogre::Vector3>(temp));
        }

        if( props.hasProperty("nearClip", &temp) ){
            camera->setNearClipDistance(any_cast<Real>(temp));
        }
    
        if( props.hasProperty("farClip", &temp) ){
            camera->setFarClipDistance(any_cast<Real>(temp));
        }

        if( props.hasProperty("fixYaw", &temp) ){
            camera->setFixedYawAxis(any_cast<bool>(temp));
        }

    }

/********************** Light Definitions ************************/
    int Light::decodePropList( const char* data, 
                               int* idx, 
                               Ymir::PropList* output )
    {
        propDecodeFP fps[] = { Ymir::OgreObject::decodeProperty,
                               Ymir::Light::decodeProperty,
                               NULL };

        return Ymir::Object::decodePropListBase( fps, data, idx, output ); 
    }
    
    int Light::decodeProperty( const string& prop, 
                               const char* args, 
                               int* idx,
                               boost::any* output ){
        int rc = 0;
        string source = "";
    
        if( prop == "source" && !decodeString(args, idx, &source) ){
           
            //Decode light source 
            if( source == "point" ){
                *output = Ogre::Light::LT_POINT;
            } else if( source == "spotlight" ){
                *output = Ogre::Light::LT_SPOTLIGHT;
            } else {
                *output = Ogre::Light::LT_DIRECTIONAL;
            }
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
    
    Ogre::MovableObject* Light::create( Ogre::SceneManager* scene,
                                        Ymir::PropList& props )
    {
        return scene->createLight(uuid);
    }
    
    Ogre::MovableObject* Light::fetch( Ogre::SceneManager* scene ){
        return scene->getLight(uuid);
    }
   
    void Light::destroy( Ogre::SceneManager* scene ){
        scene->destroyLight(uuid);
    }

    void Light::set( Ogre::SceneNode* node, 
                     Ogre::MovableObject* object, 
                     Ymir::PropList& props )
    {
        Ogre::Light* light = static_cast<Ogre::Light*>(object);
        any temp;

        setNodeCommon(node, props);

        if( props.hasProperty("source", &temp) ){
            light->setType(any_cast<Ogre::Light::LightTypes>(temp));
        }
    
        if( props.hasProperty("castShadows", &temp) ){
            light->setCastShadows(any_cast<bool>(temp));
        }
    }

/********************** Entity Definitions ************************/
    int Entity::decodePropList( const char* data, 
                               int* idx, 
                               Ymir::PropList* output )
    {
        propDecodeFP fps[] = { Ymir::OgreObject::decodeProperty,
                               Ymir::Entity::decodeProperty,
                               NULL };

        return Ymir::Object::decodePropListBase( fps, data, idx, output ); 
    } 
    
    int Entity::decodeProperty( const string& prop, 
                                const char* args, 
                                int* idx,
                                boost::any* output ){
        int rc = 0;
        string mesh = "";
    
        if( prop == "mesh" && !decodeString(args, idx, &mesh) ){
            *output = mesh;
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
    
    Ogre::MovableObject* Entity::create( Ogre::SceneManager* scene, 
                                         Ymir::PropList& props ){
        any temp;
        string mesh = "";
    
        if( !props.hasProperty("mesh", &temp) ){
            return NULL;
        }
    
        mesh = any_cast<string>(temp);
   
        return scene->createEntity(uuid, mesh);
    }
    
    Ogre::MovableObject* Entity::fetch( Ogre::SceneManager* scene ){
        return scene->getEntity(uuid);
    }
    
    void Entity::destroy( Ogre::SceneManager* scene ){
        scene->destroyEntity(uuid);
    }

    void Entity::set( Ogre::SceneNode* node,
                      Ogre::MovableObject* obj, 
                      Ymir::PropList& props )
    {
        Ogre::Entity* entity = static_cast<Ogre::Entity*>(obj);
        any temp;

        setNodeCommon(node, props);

        if( props.hasProperty("castShadows", &temp) ){
            entity->setCastShadows(any_cast<bool>(temp));
        }
    }
}
