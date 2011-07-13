#include <stdio.h>

#include "gen_cnode.h"
#include "OgreManager.h"
#include "OgreObject.h"
#include "OgreEvent.h"

using namespace std;
using namespace Ogre;

using boost::any_cast;
using namespace boost;

//References to singleton OM..used by gen_cnode and lua interaces
static OgreManager* om = NULL;

extern "C" {


/* --------------  GEN_CNODE Exports ------------------- */
GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) (om = new OgreManager());
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
    rc = ((OgreManager*)state)->start(title, paths[0],paths[1],paths[2]);
    if( rc ){
        gen_cnode_format(resp, "{error,failed}");
        goto start_exit;
    }

    gen_cnode_format(resp, "ok");

    start_exit:
    return rc;
}

GEN_CNODE_DEFINE(stop){
    ((OgreManager*)state)->stop();
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
            decodeString(args, &idx, &path) ||
            decodeString(args, &idx, &type) ||
            decodeString(args, &idx, &group) ||
            decodeBool(args, &idx, &recurse) )
        {
            rc = -EINVAL;
            gen_cnode_format(resp, "{error, einval}");
            goto exit;
        }

        ((OgreManager*)state)->addResourceLocation(path, type, group, recurse);
    }

    gen_cnode_format(resp,"ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE( initialiseAllResourceGroups ){

    ((OgreManager*)state)->initialiseAllResourceGroups();

    gen_cnode_format(resp, "ok");

    return 0;
}

//Expects a list of {uuid:string, type:string, [{prop:string, value:varies},...]}
GEN_CNODE_DEFINE( createObject ){
    int rc = 0;

    //Decode each camera object
    for( int i = 0, idx = 0; i < argc; i++ ){
        int arity = 0;
        string uuid = "";
        OgreObjectType type = OBJECT_INVALID;
        OgreObject* object = NULL;

        //UUID, type, and prop list are requred
        if( ei_decode_tuple_header(args, &idx, &arity) || 
            (arity != 3) || 
            decodeString(args, &idx, &uuid) ||
            decodeType(args, &idx, &type) )
        { 
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
            break;
        }
   
        //Create the object based on translated type
        switch( type ){
            case OBJECT_CAMERA:
                object = new OgreCamera(uuid);
                break;

            case OBJECT_LIGHT:
                object = new OgreLight(uuid);
                break;

            case OBJECT_ENTITY:
                object = new OgreEntity(uuid);
                break; 

            default:
                rc = -EINVAL;
                gen_cnode_format(resp, "{error,einval}");
                break;
        }
    
        if( object && !(object->decodeProps(args, &idx)) ){
            
           //Looks good, tell OgreManager to add the object
           ((OgreManager*)state)->createObject(object);

            gen_cnode_format(resp, "ok");
        } else {
            rc = -EINVAL;
            gen_cnode_format(resp, "{error,einval}");
        }
    }
    
    return rc;
}

//Expects a list of {uuid:string, type:string}
//and calls the appropriate scene removal function
GEN_CNODE_DEFINE( destroyObject ){
    int rc = 0;
   
    for( int i = 0, idx = 0; i < argc; i++ ){
        int arity = 0;
        OgreObjectType type = OBJECT_INVALID;
        string uuid = "";
    
        if( ei_decode_tuple_header(args, &idx, &arity) ||
            (arity != 2) ||
            decodeString(args, &idx, &uuid) ||
            decodeType(args, &idx, &type) )
        {
            rc = -EINVAL;
            break;
        }

        ((OgreManager*)state)->destroyObject(uuid, type);
    } 

    return rc;
}

GEN_CNODE_DEFINE( setViewport ){
    int rc = 0, idx = 0;
    string uuid = "";

    if( argc != 1 || decodeString(args, &idx, &uuid) ){
        rc = -EINVAL;
        goto exit;
    }

    printf("For the love of pete:  %s\n", uuid.c_str());

    ((OgreManager*)state)->setViewport(uuid);

    gen_cnode_format(resp, "ok");

    exit:
    return rc;

}

GEN_CNODE_DEFINE( addEventHandler ){

    ((OgreManager*)state)->addEventHandler(new OgreEvent());

    gen_cnode_format(resp, "ok");

    return 0;
}

GEN_CNODE_DEFINE(renderStart){
    int rc = 0;

    //Open the render window
    rc = ((OgreManager*)state)->renderStart();
    if( rc ){
        gen_cnode_format(resp, "{error, failed}");
        goto exit;
    }

    gen_cnode_format(resp, "ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE(renderStop){
    ((OgreManager*)state)->renderStop();
    
    return 0;
}
}
