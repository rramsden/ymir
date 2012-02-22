#include "LightBlueprint.h"

namespace Ymir {

    LightBlueprint::LightBlueprint() : NodeBlueprint<Ogre::Light>() {

        mBlueprint.insert(
            BPEntry("source", BPFP(&decodeLightSource, (setFP)&setLightSource)) );

    }

    int LightBlueprint::decodeLightSource( const char* data, int* idx, boost::any* output ){
       std::string temp;

        if( Ymir::decodeString(data, idx, &temp) ){
            return -EINVAL;
        } 

        //Interpret light source 
        if( temp == "point" ){
            *output = Ogre::Light::LT_POINT;
        } else if( temp == "spotlight" ){
            *output = Ogre::Light::LT_SPOTLIGHT;
        } else {
            *output = Ogre::Light::LT_DIRECTIONAL;
        }

        return 0;
    }

    void LightBlueprint::setLightSource( NodeInfo<Ogre::Light>* tup, boost::any& val ){
        tup->mObject->setType(any_cast<Ogre::Light::LightTypes>(val));
    }


    Ogre::Light* LightBlueprint::createOgreObject(std::string& id,
                                                  PropList& props,
                                                  Ogre::SceneManager* scene)
    {
        return scene->createLight(id);
    }

    Ogre::Light* LightBlueprint::findOgreObject( std::string& id, 
                                                 Ogre::SceneManager* scene)
    {
        return scene->getLight(id);
    }

    void LightBlueprint::destroyOgreObject( std::string& id, 
                                            Ogre::SceneManager* scene )
    {
        scene->destroyLight(id);
    }
}
