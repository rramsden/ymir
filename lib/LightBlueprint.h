#ifndef _LIGHTBLUEPRINT_H
#define _LIGHTBLUEPRINT_H

#include <OgreLight.h>

#include "NodeBlueprint.h"

namespace Ymir {

    class LightBlueprint : public NodeBlueprint<Ogre::Light> {

        public:
            LightBlueprint();
            ~LightBlueprint(){}

            static int decodeLightSource( const char* args,
                                      int* idx,
                                      boost::any* output );

            static void setLightSource( NodeTuple<Ogre::Light>*, boost::any& );

        protected:
            Ogre::Light* createOgreObject(std::string&,
                                          Ymir::PropList&,
                                          Ogre::SceneManager*);

            Ogre::Light* findOgreObject( std::string& id, 
                                         Ogre::SceneManager* scene);

            void destroyOgreObject( std::string&, 
                                    Ogre::SceneManager* );
    };
}



#endif
