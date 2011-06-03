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
    char paths[3][256];

    //Must provide paths to proper OGRE configuration files 
    if( argc != 3 ){
        ei_x_format(resp, "{error, einval}");
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
    rc = ((OgreManager*)state)->start(paths[0],paths[1],paths[2]);
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

GEN_CNODE_DEFINE(renderStart){
    int rc = 0, idx = 0;
    uint64_t width= 0, height = 0; 
    int fullscreen = false;
    char title[256];

    //Expect window: title, width, height, fullscreen
    if( argc != 4 ){
        rc = -1;
        ei_x_format(resp, "{error, einval}");
        goto exit;
    }

    //Clear title for good measure
    memset(title, 0x00, sizeof(title));

    //Decode in expected order
    if( ei_decode_string(args, &idx, title) ||
        ei_decode_ulong(args, &idx, &width) ||
        ei_decode_ulong(args, &idx, &height) ||
        ei_decode_boolean(args, &idx, &fullscreen) )
    {
        rc = -1;
        ei_x_format(resp, "{error, einval}");
        goto exit;
    } 

    //Open the render window
    rc = ((OgreManager*)state)->renderStart(title, width, height, fullscreen);
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
