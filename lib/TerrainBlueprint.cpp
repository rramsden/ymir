#include "TerrainBlueprint.h"

#include <OgreTerrain.h>
#include <OgreTerrainGroup.h>
#include <OgreTerrainPaging.h>
#include <OgrePagedWorld.h>

#include "Core.h"

using namespace Ogre;

namespace Ymir {

    class DummyPageProvider : public PageProvider
    {
        public:
            bool prepareProceduralPage( Page* page, PagedWorldSection* section){
                return true;
            }
            bool loadProceduralPage( Page* page, PagedWorldSection* section ){
                return true;
            }

            bool unloadProceduralPage(Page* page, PagedWorldSection* section){
                return true;
            }

            bool unprepareProceduralPage(Page* page, PagedWorldSection* section){
                return true;
            }
    };

    DummyPageProvider dummyPageProvider;

TerrainBlueprint::TerrainBlueprint() : OgreBlueprint() {

    //Terrain Group Properties
    mBlueprint["align"] = BPFP(&decodeAlign, NULL);
    mBlueprint["terrainSize"] = BPFP(&decodeULong, NULL);
    mBlueprint["worldSize"] = BPFP(&decodeReal, NULL);
    mBlueprint["prefix"] = BPFP(&decodeString, NULL);
    mBlueprint["postfix"] = BPFP(&decodeString, NULL);
    mBlueprint["origin"] = BPFP(&decodeVector3, NULL);

    //Terrain Paging Properties
    mBlueprint["loadRadius"] = BPFP(&decodeLong, NULL);
    mBlueprint["holdRadius"] = BPFP(&decodeLong, NULL);
    mBlueprint["camera"] = BPFP(&decodeString, NULL);
    mBlueprint["minX"] = BPFP(&decodeLong, NULL);
    mBlueprint["maxX"] = BPFP(&decodeLong, NULL);
    mBlueprint["minY"] = BPFP(&decodeLong, NULL);
    mBlueprint["maxY"] = BPFP(&decodeLong, NULL);
}

void createTerrainGroup( Core* core, PropList& props ){
    SceneManager* sm = core->mScene;
    Terrain::Alignment align;
    uint16 tSize;
    Real wSize;
    Vector3 origin;
    std::string prefix, postfix;

    if( !props.hasProperty<Ogre::Terrain::Alignment>("align", &align) ||
        !props.hasProperty<uint16>("terrainSize", &tSize) ||
        !props.hasProperty<Ogre::Real>("worldSize", &wSize) ||
        !props.hasProperty<Ogre::Vector3>("origin", &origin) ||
        !props.hasProperty<std::string>("prefix", &prefix)  ||
        !props.hasProperty<std::string>("postfix", &postfix) )
    {
        //<<HERE>> TODO: Raise exception
    }

    core->mTerrainGroup = OGRE_NEW Ogre::TerrainGroup(sm, align, tSize, wSize);
    core->mTerrainGroup->setFilenameConvention(prefix, postfix);
    core->mTerrainGroup->setOrigin(origin);

}

void createPageManager(Core* core, PropList& props){
    std::string cameraID;
    Ogre::Camera* camera; 

    if( !props.hasProperty<std::string>("camera", &cameraID) ||
        !(camera = core->mScene->getCamera(cameraID)) )
    {
        //<<HERE>> TODO: Raise exception
    }
    
    core->mPageManager = OGRE_NEW PageManager();

    core->mPageManager->setPageProvider(&dummyPageProvider);
    core->mPageManager->addCamera(camera);
}

void createWorld( Core* core, std::string& id, PropList& props ){
   long loadRadius, holdRadius;
   long minX = -10, maxX = 10, minY = -10, maxY = 10;
    
   if( !props.hasProperty<long>("loadRadius", &loadRadius) ||
       !props.hasProperty<long>("holdRadius", &holdRadius) )
   {
        //<<HERE>> TODO: Raise exception
   }

   //Override default min/max if specified
   props.hasProperty<long>("minX", &minX);
   props.hasProperty<long>("maxX", &maxX);
   props.hasProperty<long>("minY", &minY);
   props.hasProperty<long>("maxY", &maxY);

   core->mTerrainPaging->createWorldSection( core->mWorld, core->mTerrainGroup,
                                             loadRadius, holdRadius, 
                                             minX, minY, maxX, maxY, id );
}

void TerrainBlueprint::create(std::string& id, PropList& props){
    Core* core = Core::getSingletonPtr();
     
    //Has a world already been defined? If so, raise an exception
    if( core->mTerrainGroup ||
        core->mTerrainPaging || 
        core->mPageManager ||
        core->mWorld )
    {
        //<<HERE>> TODO: Raise exception
    } 

    //Create the terrain group
    createTerrainGroup(core, props);

    //Create page manager
    createPageManager(core, props);

    //Init and layout the world 
    core->mTerrainPaging = OGRE_NEW TerrainPaging(core->mPageManager);
    core->mWorld = core->mPageManager->createWorld();
    createWorld(core, id, props);
}

void TerrainBlueprint::update(std::string& id, PropList& props){
    //<<HERE>> TODO
}

void TerrainBlueprint::destroy(std::string& id, PropList& props){
    Core* core = Core::getSingletonPtr();

    if( core->mWorld ){
        OGRE_DELETE core->mWorld;
    }

    if( core->mPageManager ){
        OGRE_DELETE core->mPageManager;
    }

    if( core->mTerrainPaging ){
        OGRE_DELETE core->mTerrainPaging;
    }

    if( core->mTerrainGroup ){
        OGRE_DELETE core->mTerrainGroup;
    }

    core->mWorld = NULL;
    core->mPageManager = NULL;
    core->mTerrainPaging = NULL;
    core->mTerrainGroup = NULL; 
}

int  TerrainBlueprint::decodeAlign(const char* data, 
                                   int* idx, 
                                   boost::any* output)
{
    std::string temp = "";

    if( Ymir::decodeString(data, idx, &temp) ){
        return -EINVAL;
    }

    if( temp == "align_x_z" ){
        *output = Ogre::Terrain::ALIGN_X_Z;
    } else if( temp == "align_x_y" ){
        *output = Ogre::Terrain::ALIGN_X_Y;
    } else if( temp == "align_y_z" ){
        *output = Ogre::Terrain::ALIGN_Y_Z;
    } else {
        return -EINVAL;
    } 

    return 0;
}

}
