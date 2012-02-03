#include "TerrainBlueprint.h"

#include <OgreTerrain.h>
#include <OgreTerrainGroup.h>
#include <OgreTerrainPaging.h>
#include <OgrePagedWorld.h>

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
    mBlueprint["terrainSize"] = BPFP(&decodeUInt16, NULL);
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

void TerrainBlueprint::createTerrainGroup( Core* core, PropList& props ){
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

void TerrainBlueprint::createPageManager(Core* core, PropList& props){
    std::string cameraID;
    Ogre::Camera* camera = NULL; 

    if( !props.hasProperty<std::string>("camera", &cameraID) ||
        !(camera = core->mScene->getCamera(cameraID)) )
    {
        //<<HERE>> TODO: Raise exception
    }
    
    core->mPageManager = OGRE_NEW PageManager();

    core->mPageManager->setPageProvider(&dummyPageProvider);
    core->mPageManager->addCamera(camera);
}

void TerrainBlueprint::createWorld( Core* core, 
                                    std::string& id, 
                                    PropList& props )
{
   long loadRadius = 2000, holdRadius = 3000;
   //long minX = 0, maxX = 0, minY = 0, maxY = 0;
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

void TerrainBlueprint::configureTerrainGlobals( Core* core, 
                                                PropList& props )
{
    
    TerrainGlobalOptions* options = core->mTerrainGlobals;

    options->setMaxPixelError(8);

    options->setCompositeMapDistance(3000);

    options->setLightMapDirection(Vector3(0.55, -0.3, 0.75));

    options->setCompositeMapAmbient(ColourValue(0.2, 0.2, 0.2));
    options->setCompositeMapDiffuse(ColourValue::White);

    Terrain::ImportData& defaultimp = core->mTerrainGroup->getDefaultImportSettings();

    defaultimp.terrainSize = 513;
    defaultimp.worldSize = 12000.0f;
    defaultimp.inputScale = 600;
    defaultimp.minBatchSize = 33;
    defaultimp.maxBatchSize = 65;

    defaultimp.layerList.resize(3);
    defaultimp.layerList[0].worldSize = 100;
    defaultimp.layerList[0].textureNames.push_back("dirt_grayrocky_diffusespecular.dds");
    defaultimp.layerList[0].textureNames.push_back("dirt_grayrocky_normalheight.dds");
    defaultimp.layerList[1].worldSize = 30;
    defaultimp.layerList[1].textureNames.push_back("grass_green-01_diffusespecular.dds");
    defaultimp.layerList[1].textureNames.push_back("grass_green-01_normalheight.dds");
    defaultimp.layerList[2].worldSize = 200;
    defaultimp.layerList[2].textureNames.push_back("growth_weirdfungus-03_diffusespecular.dds");
    defaultimp.layerList[2].textureNames.push_back("growth_weirdfungus-03_normalheight.dds"); 
}

void TerrainBlueprint::create(std::string& id, PropList& props){
    Core* core = Core::getSingletonPtr();
    

    //Has a world already been defined? If so, raise an exception
    if( !core->mScene ||
        core->mTerrainGlobals ||
        core->mTerrainGroup ||
        core->mTerrainPaging || 
        core->mPageManager ||
        core->mWorld )
    {
        //<<HERE>> TODO: Raise exception
    } 

    MaterialManager::getSingleton().setDefaultTextureFiltering(TFO_ANISOTROPIC);
    MaterialManager::getSingleton().setDefaultAnisotropy(7);

    //Setup terrain globals
    core->mTerrainGlobals = OGRE_NEW TerrainGlobalOptions();

    //Create the terrain group
    createTerrainGroup(core, props);

    //configureTerrainGlobals(core, props);

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

    if( core->mTerrainPaging ){
        OGRE_DELETE core->mTerrainPaging;
    }

    if( core->mPageManager ){
        OGRE_DELETE core->mPageManager;
    }

    if( core->mTerrainGroup ){
        OGRE_DELETE core->mTerrainGroup;
    }

    if( core->mTerrainGlobals ){
        OGRE_DELETE core->mTerrainGlobals;
    }

    core->mTerrainPaging = NULL;
    core->mPageManager = NULL;
    core->mTerrainGroup = NULL; 
    core->mWorld = NULL;
    core->mTerrainGlobals = NULL;
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
