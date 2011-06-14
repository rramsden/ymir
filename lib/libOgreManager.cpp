#include <stdio.h>

#include "gen_cnode.h"
#include "OgreManager.h"

using namespace std;
using namespace Ogre;

//References to singleton OM..used by gen_cnode and lua interaces
static OgreManager* om = NULL;

extern "C" {

/* -------------- LUA Exports ---------------------*/





/* --------------  GEN_CNODE Exports ------------------- */
GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) (om = new OgreManager());
}

GEN_CNODE_DEFINE(start){
    int rc = 0;
    int i, idx;
    char title[256];
    char paths[3][256];

    //Must provide paths to proper OGRE configuration files 
    if( argc != 4 ){
        ei_x_format(resp, "{error, einval}");
    }
   
    memset(title, 0x00, 256);
    if( ei_decode_string(args, &idx, title) ){
        rc = -1;
        ei_x_format(resp, "{error, einval}");
        goto start_exit;
    }

    //Expects 3 filepath strings in the following order:
    //  plugin_file, config_file, lofe_file 
    for( i=0, idx=0; i < 3; i++ ){

        //Clear memory
        memset(paths[i], 0x00, 256);

        //Decode our string arg
        if( ei_decode_string(args, &idx, paths[i]) ){
            rc = -1;
            ei_x_format(resp, "{error, einval}");
            goto start_exit;
        }
    }

    //All seems well, start OGRE
    rc = ((OgreManager*)state)->start(title, paths[0],paths[1],paths[2]);
    if( rc ){
        ei_x_format(resp, "{error,failed}");
        goto start_exit;
    }

    ei_x_format(resp, "ok");

    start_exit:
    return rc;
}

GEN_CNODE_DEFINE(stop){
    ((OgreManager*)state)->stop();
    ei_x_format(resp,"ok");
    return 0;
}

/**
 * Loads the given Ymir module.
 */
GEN_CNODE_DEFINE( loadModule ){
    int rc = 0, idx = 0;
    char path[256];

    //Expect path to a module folder
    if( argc != 1 ){
        rc = -EINVAL;
        ei_x_format(resp,"{error,einval}");
        goto exit;
    }

    //Decode path to module resources path
    memset(path, 0x00, sizeof(path));
    if( ei_decode_string(args, &idx, path) ){
        rc = -EINVAL;
        ei_x_format(resp,"{error,einval}");
        goto exit;
    }
  
    rc = ((OgreManager*)state)->loadModule(path); 
    if( rc ){
        ei_x_format(resp, "{error, failed}");
        goto exit;
    }

    exit:
    return rc;
}


GEN_CNODE_DEFINE(renderStart){
    int rc = 0;

    //Open the render window
    rc = ((OgreManager*)state)->renderStart();
    if( rc ){
        ei_x_format(resp, "{error, failed}");
        goto exit;
    }

    ei_x_format(resp, "ok");

    exit:
    return rc;
}

GEN_CNODE_DEFINE(renderStop){
    ((OgreManager*)state)->renderStop();
    
    return 0;
}
}
