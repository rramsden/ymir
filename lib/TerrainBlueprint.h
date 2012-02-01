#ifndef _TERRAINBLUEPRINT_H
#define _TERRAINBLUEPRINT_H

#include "OgreBlueprint.h"

#include "Core.h"

namespace Ymir {
class TerrainBlueprint : public OgreBlueprint {

    public:
        TerrainBlueprint();
        ~TerrainBlueprint(){}
    
        void create(std::string&, PropList&);
        void update(std::string&, PropList&);
        void destroy(std::string&, PropList&);

        static int decodeAlign( const char*, int*, boost::any*);

    protected:
        void createTerrainGroup( Core* core, PropList& props );
        void createPageManager(Core* core, PropList& props);
        void createWorld( Core* core, std::string& id, PropList& props );
};
}
#endif
