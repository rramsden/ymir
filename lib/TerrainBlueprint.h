#ifndef _TERRAINBLUEPRINT_H
#define _TERRAINBLUEPRINT_H

#include "OgreBlueprint.h"

namespace Ymir {
class TerrainBlueprint : public OgreBlueprint {

    public:
        TerrainBlueprint();
        ~TerrainBlueprint(){}
    
        void create(std::string&, PropList&);
        void update(std::string&, PropList&);
        void destroy(std::string&, PropList&);

        static int decodeAlign( const char*, int*, boost::any*);
};
}
#endif
