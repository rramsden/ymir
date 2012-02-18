#include <stdio.h>

#include "gen_cnode.h"

#include "DecodeBasic.h"
#include "Core.h"
#include "ObjectFactory.h"
#include "OgreEventListener.h"

using namespace std;
using namespace Ogre;

using boost::any_cast;
using namespace boost;
using namespace Ymir;

//References to singleton OM..used by gen_cnode and lua interaces
static Core* core = NULL;

extern "C" {


/* --------------  GEN_CNODE Exports ------------------- */
GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) (core = Ymir::Core::getSingletonPtr());
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
        bool recurse = 0;
        string type = "", 
               path = "", 
               group = "";

        //Each arg must be of the form {path, type, group, recursive}
        //or {string, string, string, boolean} respectively 
        if( ei_decode_tuple_header(args, &idx, &arity) || arity != 4 ||
            Ymir::decodeString(args, &idx, &path) ||
            Ymir::decodeString(args, &idx, &type) ||
            Ymir::decodeString(args, &idx, &group) ||
            Ymir::decodeBool(args, &idx, &recurse) )
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

/*GEN_CNODE_DEFINE( initialiseMyGUI ){
    int idx = 0;
    string config = "";

    if( (argc != 1) || Ymir::decodeString(args, &idx, &config) ){
        gen_cnode_format(resp, "{error, einval}");
        return -EINVAL;
    }

    ((Core*)state)->initialiseMyGUI(config);

    gen_cnode_format(resp, "ok");
    return 0;
}*/

//Expects a list of {uuid:string, type:string, [{prop:string, value:varies},...]}
GEN_CNODE_DEFINE( create ){
    int rc = 0;

    //Decode each object
    for( int i = 0, idx = 0; !rc && (i < argc); i++ ){
        string uuid = "";
        Ymir::Object::Type type = Ymir::Object::Invalid;
        Ymir::PropList props = Ymir::PropList();

        //UUID, type, and prop list are requred
        if( ObjectFactory::decodeObject(args, &idx, &uuid, &type, &props) ){ 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }

        try {
            core->create(uuid, type, props);
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
        string uuid = "";
        Ymir::Object::Type type = Ymir::Object::Invalid;
        Ymir::PropList props = Ymir::PropList();

        //UUID, type, and prop list are requred
        if( ObjectFactory::decodeObject(args, &idx, &uuid, &type, &props) ){ 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }
  
        try {
            core->update(uuid, type, props);
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

    //Decode each update object
    for( int i = 0, idx = 0; !rc && (i < argc); i++ ){
        string uuid = "";
        Ymir::Object::Type type = Ymir::Object::Invalid;
        Ymir::PropList props = Ymir::PropList();

        //UUID, type, and prop list are requred
        if( ObjectFactory::decodeObject(args, &idx, &uuid, &type, &props) ){ 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, failed_to_decode}");
            break;
        }
  
        try {
            core->destroy(uuid, type, props);
        }

        catch( ... ){
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, exception}");
        }
    }
   
    if( !rc ){
       gen_cnode_format(resp, "ok"); 
    }

    return rc;
}

/*GEN_CNODE_DEFINE( setViewport ){
    int rc = 0, idx = 0;
    string uuid = "";

    if( argc != 1 || Ymir::decodeString(args, &idx, &uuid) ){
        rc = -EINVAL;
        goto exit;
    }

    ((Core*)state)->setViewport(uuid);

    gen_cnode_format(resp, "ok");

    exit:
    return rc;
}*/

GEN_CNODE_DEFINE( addEventHandler ){

    ((Core*)state)->addEventListener(new OgreEventListener());

    gen_cnode_format(resp, "ok");

    return 0;
}

GEN_CNODE_DEFINE(ticktock){
    if( ((Core*)state)->ticktock() < 0 ){
        gen_cnode_format(resp, "{error, failed}");      
        return -EINVAL;
    }

    gen_cnode_format(resp, "ok");
    return 0;
}

/*GEN_CNODE_DEFINE(renderStart){
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
}*/
}
