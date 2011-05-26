#include <OGRE/Ogre.h>

#include "gen_cnode.h"

using namespace std;
using namespace Ogre;

class OgreManager {

    public:
        OgreManager();
        ~OgreManager();
        int start(ei_x_buff* resp);
    
    protected:
        Root* root;
        RenderWindow* window;
};

OgreManager::OgreManager(){
    root = NULL;
    window = NULL;
}

OgreManager::~OgreManager(){

}

int OgreManager::start(ei_x_buff* resp){

    if( !root ){
        root = new Root("/etc/OGRE/plugins.cfg");
    }

    if( root->showConfigDialog() ){
        ei_x_format(resp, "ok");
    } else {
        ei_x_format(resp, "canceled");
    }

    return 0;
}

#ifdef __cplusplus
extern "C" {
#endif

GEN_CNODE_STATE_NEW() {
    return (struct gen_cnode_lib_state_s *) new OgreManager();
}

GEN_CNODE_DEFINE(start){
    return ((OgreManager*)state)->start(resp);
}

#ifdef __cplusplus
}
#endif
