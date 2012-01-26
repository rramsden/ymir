#include <errno.h>

//#include "Core.h"
#include "OgreObject.h"

#include "Core.h"

using namespace std;
using namespace boost;
using namespace Ogre;

namespace Ymir {


/********************** Terrain Definitions ************************/

    int decodeAlign( const char* data, int* idx, Ogre::Terrain::Alignment* out ){
        std::string temp = "";

        if( Ymir::Object::decodeString(data, idx, &temp) ){
            return -EINVAL;
        }

        if( temp == "align_x_z" ){
            *out = Ogre::Terrain::ALIGN_X_Z;
        } else if ( temp == "align_x_y" ){
            *out = Ogre::Terrain::ALIGN_X_Y;
        } else if( temp == "align_y_z" ){
            *out = Ogre::Terrain::ALIGN_Y_Z;
        } else {
            return -EINVAL;
        }

        return 0;
    }
                    

    /*int decodeGridLoc( const char* data, 
                       int* idx, 
                       std::pair<long, long>* output )
    {
        int arity = 0;
        long x, y;

        if( ei_decode_tuple_header(data, idx, &arity) || 
            (arity != 2) ||
            Ymir::Object::decodeLong(data, idx, &x) ||
            Ymir::Object::decodeLong(data, idx, &y) )
        {
            return -EINVAL;
        }
        
        output->first = x;
        output->second = y;

        return 0;
    }

    int decodeLayer( const char* data,
                     int* idx, 
                     Ogre::Terrain::LayerInstance* output )
    {
        int arity = 0, count = 0;
        Ogre::Real size;
        std::vector<std::string> textures;
        Ogre::Terrain::LayerInstanceList layers;

        if( ei_decode_tuple_header(data, idx, &arity) || 
            (arity != 2) ||
            Ogre::Object::decodeReal(data, idx, &size) ||
            decodeVector<std::string>(data, idx, decodeString, &textures) )
        {
            return -EINVAL;
        }


        output->worldSize = size;
        output->textureNames = textures;

        return 0;
    }*/

    void Terrain::create( Ymir::PropList& props ){
        Ogre::TerrainGroup* tg = NULL;
        boost::any align, tSize, wSize, pre, post, origin;
        //std::list<std::pair<long, long> >::iterator itPages;
        //std::list<std::pair<long, long> > pages;

        //Required:  SceneManager, Alignment, Size, WorldSize
        
        //<<HERE>> For now assuming just one active scene, this may need to
        //change
        Ogre::SceneManager* sm = Ymir::Core::getSingletonPtr()->getScene();
       
        if( !props.hasProperty("alignment", &align) &&
            !props.hasProperty("terrainSize", &tSize) &&
            !props.hasProperty("worldSize", &wSize) &&
            !props.hasProperty("prefix", &pre) &&
            !props.hasProperty("postfix", &post) &&
            !props.hasProperty("origin", &origin) )
        {
            //<<HERE>> Error!
            return;
        }
       
        ptr = tg =  new Ogre::TerrainGroup( sm,
                                            boost::any_cast<Ogre::Terrain::Alignment>(align),
                                            boost::any_cast<uint16>(tSize),
                                            boost::any_cast<Ogre::Real>(wSize) );

        tg->setOrigin(boost::any_cast<Ogre::Vector3>(origin));
        tg->setFilenameConvention(boost::any_cast<std::string>(pre),
                                  boost::any_cast<std::string>(post));


        set(props);


        /*pages = boost::any_cast<std::list<std::pair<long, long> > >(tPages);
        for( itPages = pages.begin(); itPages != pages.end(); itPages++ ){
            long x = *itPages->first, y = *itPages->second;
            Ogre::String filename = tg->generateFilename(x, y);
            Ogre::ResourceGroupManager* rgm = Ogre::ResrouceGroupManager::getSingletonPtr();

            //Does the heightmap data exists for this tile already?
            if( rgm->resourceExists(tg->getResrouceGroup(), filename) ){
                tg->defineTerrain(x, y); //If so, simply load it
            } 
        }*/
    }

    void Terrain::update( Ymir::PropList& props ){

       //set(props); 
    }


    //<<HERE>> TODO
    void Terrain::destroy( ){

        

    }

    int Terrain::decodePropList( const char* data, 
                                 int* idx, 
                                 Ymir::PropList* output )
    {
        propDecodeFP fps[] = { Ymir::Terrain::decodeProperty,
                               NULL };

        return Ymir::Object::decodePropListBase( fps, data, idx, output ); 
    }

    int Terrain::decodeProperty( const string& prop, 
                                 const char* data, 
                                 int* idx,
                                 boost::any* output )
    {
        long iSize;
        std::string str = "";
        std::list<std::pair<long,long> > pages;
        std::vector<Ogre::Terrain::LayerInstance> layers;
        Ogre::Terrain::Alignment align;
        Ogre::Vector3 vec3;
        Ogre::Real rSize;

        if( prop == "prefix" && !decodeString(data, idx, &str) ){
            *output = str;
        } else if ( prop == "postfix" && !decodeString(data, idx, &str) ){
            *output = str;
        } else if( prop == "origin" && !OgreObject::decodeVector3(data, idx, &vec3) ){
            *output = vec3;
        } else if ( prop == "align" && !decodeAlign(data, idx, &align) ){
            *output = align;
        } else if ( prop == "terrainSize" && !decodeLong(data, idx, &iSize) ){
            *output = (unsigned short) iSize;
        } else if( prop == "worldSize" && !OgreObject::decodeReal(data, idx, &rSize) ){
            *output = rSize;
        } else if( prop == "resourceGroup" && !decodeString(data, idx, &str) ){
            *output = str;
        } 
        
        /*else if( prop == "layers" && !decodeVector<Ogre::Terrain::LayerInstance> >(data, idx, decodeLayer, &layers))
        {
            *output = layers;
        } else if( prop == "pages" && !decodeList<std::pair<long, long> >(data, idx, decodeGridLoc, &pages)) 
        {
            *output = pages; 
        }*/ else {
            return -EINVAL;
        }

        return 0;
    }

    void Terrain::set(Ymir::PropList& props){
        boost::any temp;
        Ogre::TerrainGroup* tg = static_cast<Ogre::TerrainGroup*>(ptr);
        /*Ogre::Terrain::ImportData& imp = tg->getDefaultImportSettings();

        if( props.hasProperty("origin", &temp) ){
            tg->setOrigin(boost::any_cast<Ogre::Vector3>(temp));
        }*/
    
        if( props.hasProperty("resourceGroup", &temp) ){
            tg ->setResourceGroup(boost::any_cast<std::string>(temp));
        }

        /*if( props.hasProperty("inputScale", &temp) ){
            im.inputScale = boost::any_cast<long>

        }

        if( props.hasProperty("layers", &temp) ){
            imp.layerList = boost::any_cast<Ogre::Terrain::LayerInstanceList>(temp);
        }

        if( props.hasProperty("pages", &temp) ){


        }*/
    }


/********************** OgreObject Definitions ************************/
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
