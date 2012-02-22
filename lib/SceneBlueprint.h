#ifndef _SCENEBLUEPRINT_H
#define _SCENEBLUEPRINT_H

#include "OgreBlueprint.h"

namespace Ymir {

class SceneBlueprint : public OgreBlueprint {

    public:
        SceneBlueprint();
        ~SceneBlueprint();

        //Behaviors from OgreObject
        void create( std::string& id, PropList& props );
        void update( std::string& id, PropList& props );
        void destroy( std::string& id, PropList& props );

        //Factory like behavior for CrUD of scene objects
        static int decodeTerrain( const char*, int*, boost::any* );
        static int decodeSceneType( const char*, int*, boost::any* ); 
        static int decodeObject( const char*, int*, boost::any* );
        static int decodeObjects( const char*, int*, boost::any* );

        //static void setFog( Ogre::SceneManager*, boost::any& );
        static void setAmbient( Ogre::SceneManager*, boost::any& );
        static void setDebug( Ogre::SceneManager*, boost::any& );
        static void setGravity( Ogre::SceneManager*, boost::any& );

    protected:
        void createSceneManager( std::string& id, PropList& props );
        void createPhysicsSim( PropList& props );
        void createViewport( std::string& id, PropList& props );
        void createGUI();
};

}
#endif
