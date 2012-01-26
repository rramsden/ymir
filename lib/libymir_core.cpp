#include <stdio.h>

#include "gen_cnode.h"

#include "Core.h"

#include "OgreObject.h"
#include "MyGUIObject.h"

#include "OgreEventListener.h"

using namespace std;
using namespace Ogre;

using boost::any_cast;
using namespace boost;
using namespace Ymir;

//References to singleton OM..used by gen_cnode and lua interaces
static Core* om = NULL;

int decodeObject( const char* args, 
                  int* idx, 
                  std::string* uuid,
                  Ymir::Object::Type* type,
                  Ymir::PropList* props )
{
    int rc = 0;

    if( Ymir::Object::decodeString(args, idx, uuid) ||
        Ymir::Object::decodeType(args, idx, type) ||
        (*type <= Ymir::Object::Invalid) || 
        (*type >= Ymir::Object::Max) )
    {
        return -EINVAL;
    }

    switch( *type ){
   
        case Ymir::Object::Terrain:
            rc = Ymir::Terrain::decodePropList(args, idx, props);

        case Ymir::Object::Camera:
            rc = Ymir::Camera::decodePropList(args, idx, props);
            break;

        case Ymir::Object::Light:
            rc = Ymir::Light::decodePropList(args, idx, props);
            break;

        case Ymir::Object::Entity:
            rc = Ymir::Entity::decodePropList(args, idx, props);
            break;

        case Ymir::Object::Window:
            rc = Ymir::Window::decodePropList(args, idx, props);
            break;

        case Ymir::Object::Button:
            rc = Ymir::Button::decodePropList(args, idx, props);
            break;

        default:
            //Never reached
            break;
    }   

    return rc;
}

extern "C" {


/* --------------  GEN_CNODE Exports ------------------- */
GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) (om = Ymir::Core::getSingletonPtr());
}

GEN_CNODE_DEFINE(start){
    int rc = 0;
    int i, idx = 0;
    char title[256] = {0};
    char paths[3][256];

    //Must provide paths to proper OGRE configuration files 
    if( argc != 4 ){
        rc = -EINVAL;
        gen_cnode_format(resp, "{error, einval}");
        goto start_exit;
    }
   
    memset(title, 0x00, 256);
    if( ei_decode_string(args, &idx, title) ){
        rc = -EINVAL;
        gen_cnode_format(resp, "{error, einval}");
        goto start_exit;
    }

    //Expects 3 filepath strings in the following order:
    //  plugin_file, config_file, lofe_file 
    for( i=0; i < 3; i++ ){

        //Clear memory
        memset(paths[i], 0x00, 256);

        //Decode our string arg
        if( ei_decode_string(args, &idx, paths[i]) ){
            rc = -1;
            gen_cnode_format(resp, "{error, einval}");
            goto start_exit;
        }
    }

    //All seems well, start OGRE
    rc = ((Core*)state)->start(title, paths[0],paths[1],paths[2]);
    if( rc ){
        gen_cnode_format(resp, "{error,failed}");
        goto start_exit;
    }

    gen_cnode_format(resp, "ok");

    start_exit:
    return rc;
}

GEN_CNODE_DEFINE(stop){
    ((Core*)state)->stop();
    gen_cnode_format(resp,"ok");
    return 0;
}

GEN_CNODE_DEFINE( addResourceLocation ){
    int rc = 0;
    int i = 0, idx = 0;

    if( !argc ){
        rc = -EINVAL;
        gen_cnode_format(resp, "{error, einval}");
        goto exit;
    }

    //Load all resource locations given
    for( i=0; i < argc; i++ ){
        int arity = 0;
        int recurse = 0;
        string type = "", 
               path = "", 
               group = "";

        //Each arg must be of the form {path, type, group, recursive}
        //or {string, string, string, boolean} respectively 
        if( ei_decode_tuple_header(args, &idx, &arity) || arity != 4 ||
            Ymir::Object::decodeString(args, &idx, &path) ||
            Ymir::Object::decodeString(args, &idx, &type) ||
            Ymir::Object::decodeString(args, &idx, &group) ||
            Ymir::Object::decodeBool(args, &idx, &recurse) )
        {
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, einval}");
            goto exit;
        }

        ((Core*)state)->addResourceLocation(path, type, group, recurse);
    }

    gen_cnode_format(resp,"ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE( initialiseAllResourceGroups ){

    ((Core*)state)->initialiseAllResourceGroups();

    gen_cnode_format(resp, "ok");

    return 0;
}

GEN_CNODE_DEFINE( initialiseMyGUI ){
    int idx = 0;
    string config = "";

    if( (argc != 1) || Ymir::Object::decodeString(args, &idx, &config) ){
        gen_cnode_format(resp, "{error, einval}");
        return -EINVAL;
    }

    ((Core*)state)->initialiseMyGUI(config);

    gen_cnode_format(resp, "ok");
    return 0;
}

//Expects a list of {uuid:string, type:string, [{prop:string, value:varies},...]}
GEN_CNODE_DEFINE( create ){
    int rc = 0;

    //Decode each object
    for( int i = 0, idx = 0; !rc && (i < argc); i++ ){
        int arity = 0;
        string uuid = "";
        Ymir::Object::Type type = Ymir::Object::Invalid;
        Ymir::PropList props = Ymir::PropList();

        //UUID, type, and prop list are requred
        if( ei_decode_tuple_header(args, &idx, &arity) || 
            (arity != 3) ||
            decodeObject(args, &idx, &uuid, &type, &props) ) 
        { 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }

        try {
            om->create(uuid, type, props);
        }

        catch( Ogre::Exception e ){
            rc = -EINVAL;
            gen_cnode_format(resp, "{exception, %s}", e.what());
        }
    }

    if( !rc ){
        gen_cnode_format(resp, "ok");
    }

    return rc;
}

GEN_CNODE_DEFINE( update ){
    int rc = 0;

    //Decode each update object
    for( int i = 0, idx = 0; !rc && (i < argc); i++ ){
        int arity = 0;
        string uuid = "";
        Ymir::Object::Type type = Ymir::Object::Invalid;
        Ymir::PropList props = Ymir::PropList();

        //UUID, type, and prop list are requred
        if( ei_decode_tuple_header(args, &idx, &arity) || 
            (arity != 3) || 
            decodeObject(args, &idx, &uuid, &type, &props) )
        { 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }
  
        try {
            om->update(uuid, props);
        }

        catch( ... ){
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, einval}");
        }
    }
   
    if( !rc ){
       gen_cnode_format(resp, "ok"); 
    }

    return rc;
}

//Expects a list of uuid:string
//and calls the appropriate scene removal function
GEN_CNODE_DEFINE( destroy ){
    int rc = 0;
   
    for( int i = 0, idx = 0; !rc && (i < argc); i++ ){
        string uuid = "";

        if( Ymir::Object::decodeString(args, &idx, &uuid) )
        {
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }

        try {
            om->destroy(uuid);
        }

        catch( ... ){
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, could_not_destroy}");
        }
    }

    if( !rc ){
        gen_cnode_format(resp, "ok");
    } 
    
    return rc;
}

GEN_CNODE_DEFINE( setViewport ){
    int rc = 0, idx = 0;
    string uuid = "";

    if( argc != 1 || Ymir::Object::decodeString(args, &idx, &uuid) ){
        rc = -EINVAL;
        goto exit;
    }

    ((Core*)state)->setViewport(uuid);

    gen_cnode_format(resp, "ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE( addEventHandler ){

    ((Core*)state)->addEventListener(new OgreEventListener());

    gen_cnode_format(resp, "ok");

    return 0;
}

GEN_CNODE_DEFINE(renderStart){
    int rc = 0;

    //Open the render window
    rc = ((Core*)state)->renderStart();
    if( rc ){
        gen_cnode_format(resp, "{error, failed}");
        goto exit;
    }

    gen_cnode_format(resp, "ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE(renderStop){
    ((Core*)state)->renderStop();
    gen_cnode_format(resp, "ok");
    return 0;
}
}
