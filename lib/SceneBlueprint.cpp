#include "SceneBlueprint.h"

#include "Core.h"
#include "ObjectFactory.h"

using namespace Ogre;
using namespace MyGUI;

namespace Ymir{

    SceneBlueprint::SceneBlueprint() : OgreBlueprint() {

        //Ogre scene properties
        mBlueprint["type"] = BPFP(&decodeSceneType, NULL);
        mBlueprint["terrain"] = BPFP(&decodeTerrain,  NULL);
        mBlueprint["objects"] = BPFP(&SceneBlueprint::decodeObjects, NULL);
        
        mBlueprint["ambient"] = BPFP(&decodeColourVal, (setFP)&setAmbient);
        mBlueprint["viewport"] = BPFP(&decodeString, NULL);
        //mBlueprint["fog"] = BPFP(&decodeFog, (setFP)&setFog);

        //Physics properties


    }

    SceneBlueprint::~SceneBlueprint(){}

    void SceneBlueprint::createPhysicsSim( PropList& props ){
        Core* core = Core::getSingletonPtr();      
        
        core->mBroadphase = new btDbvtBroadphase();
        
        core->mCollisionConfig = new btDefaultCollisionConfiguration();
        
        core->mCollisionDispatcher = 
            new btCollisionDispatcher(core->mCollisionConfig);
        
        core->mConstraintSolver = new btSequentialImpulseConstraintSolver;
        
        core->mDynamicsWorld = 
            new btDiscreteDynamicsWorld( core->mCollisionDispatcher,
                                         core->mBroadphase,
                                         core->mConstraintSolver,
                                         core->mCollisionConfig );
        
        
    }

    void SceneBlueprint::createSceneManager( std::string& id, PropList& props ){
        Core* core = Core::getSingletonPtr();
        Ogre::SceneType type = Ogre::ST_GENERIC;

        //Create the SceneManager
        props.hasProperty<Ogre::SceneType>("type", &type);
        core->mScene = core->root->createSceneManager(type, id);
    }

    void SceneBlueprint::createViewport(std::string& id, PropList& props){
        Core* core = Core::getSingletonPtr();
        std::string cameraID = id + "_camera";
        std::list<Object>::iterator it;
        std::list<Object> objects;

        props.hasProperty<std::string>("viewport", &cameraID);
        props.hasProperty<std::list<Object> >("objects", &objects);

        //Look for the indicated camera
        for( it = objects.begin(); it != objects.end(); it++ ){

            if( it->id == cameraID && it->type == Object::Camera ){
                ObjectFactory::create(it->id, it->type, it->props);
                break;
            }
        }

        if( it != objects.end() ){ //If found, erase it.
            objects.erase(it);
            props["objects"] = objects;

        } else {  //Otherwise just create a default camera
            PropList props;
            ObjectFactory::create(cameraID, Object::Camera, props);
        } 
        
        //Fetch the camera and create the viewport
        Camera* camera = core->mScene->getCamera(cameraID);
        core->viewport = core->window->addViewport(camera); 
    }

    void SceneBlueprint::createGUI(){
        Core* core = Core::getSingletonPtr();

        core->platform = new OgrePlatform();
        core->platform->initialise(core->window, core->mScene);

        core->gui = new Gui();
        core->gui->initialise();
    }

    void SceneBlueprint::create( std::string& id, PropList& props ){
        Core* core = Core::getSingletonPtr();

        core->logNormal("Creating scene: " + id);

        if( core->mScene ){ //Scene already active, cleanup first
            std::string oldId = core->mScene->getName();
            PropList temp = PropList();
            destroy(oldId, temp);
        }

        //First things, first.  Setup the scene and attach gui to it
        createSceneManager(id, props);

        //Setup the physics environment
        createPhysicsSim(props);



        //Before creating objects, we first must look ahead
        //for a defined viewport.  This is a requirement of the MyGUI
        //subsystem. If no objects are defined or the viewport is not
        //defined then one will be created automatically.
        createViewport(id, props);

        //Before creating objects we also must initialise MyGUI
        createGUI();

        //Every scene definition may have a set of objects defined in 
        //advance.  Only restriction is that a scene my not contain
        //another scene. 
        std::list<Object> objects;
        if( props.hasProperty<std::list<Object> >("objects", &objects) ){
            std::list<Object>::iterator it;

            //One by one create the objects specified
            for( it = objects.begin(); it != objects.end(); it++ ){

                if( it->type == Ymir::Object::Scene ){
                    //TODO: Throw exception
                    continue;
                }

                core->logNormal("Creating object: " + it->id);

                ObjectFactory::create(it->id, it->type, it->props);
            }
        }

        //If a lanscape is defined, create it
        PropList tProps;
        if( props.hasProperty<PropList>("terrain", &tProps) ){
            std::string terrain_id = id + "_terrain";
            ObjectFactory::create(terrain_id, Object::Terrain, tProps);
        } 

        set(core->mScene, props);
    }

    void SceneBlueprint::update( std::string& id, PropList& props ){



    }

    void SceneBlueprint::destroy( std::string& id, PropList& props ){
        Core* core = Core::getSingletonPtr();

        //Clear terrain, if any <<HERE>> Better way?
        ObjectFactory::destroy(id, Object::Terrain, props);
       
        if( core->gui ){
            core->gui->shutdown();
            delete core->gui;
            core->gui = NULL;
        }

        if( core->platform ){
            core->platform->shutdown();
            delete core->platform;
            core->platform = NULL;
        }

        //Destroy scene itself
        if( core->mScene ){
            core->window->removeAllViewports();
            core->mScene->clearScene();
            core->root->destroySceneManager(core->mScene);
            core->mScene = NULL;
        }

        core->logNormal("Destroying bullet simulation...");

        //Clean up physics simulation
        if( core->mDynamicsWorld ){
            delete core->mDynamicsWorld;
            core->mDynamicsWorld = NULL;
        }

        if( core->mConstraintSolver ){
            delete core->mConstraintSolver;
            core->mConstraintSolver = NULL;
        }

        if( core->mCollisionDispatcher ){
            delete core->mCollisionDispatcher;
            core->mCollisionDispatcher = NULL;  
        }

        if( core->mCollisionConfig ){
            delete core->mCollisionConfig;
            core->mCollisionConfig = NULL;
        }

        if( core->mBroadphase ){
            delete core->mBroadphase;
            core->mBroadphase = NULL;
        }
    }

    int SceneBlueprint::decodeSceneType( const char* data,
                                         int* idx, 
                                         boost::any* out )
    {
        std::string temp = "";

        if( Ymir::decodeString(data, idx, &temp) ){
            return -EINVAL;
        }

        if( temp == "exterior_small" ){
            *out = Ogre::ST_EXTERIOR_CLOSE;
        } else if( temp == "exterior_medium" ){    
            *out = Ogre::ST_EXTERIOR_FAR;
        } else if( temp == "exterior_large" ){
            *out = Ogre::ST_EXTERIOR_REAL_FAR;
        } else if( temp == "interior" ){
            *out = Ogre::ST_INTERIOR; 
        } else {
            *out = Ogre::ST_GENERIC;
        }

        return 0;
    }

    int SceneBlueprint::decodeTerrain( const char* data,
                                       int* idx, 
                                       boost::any* out )
    {
        std::string id = "Terrain";
        PropList props;

        if( ObjectFactory::decode(data, idx, Object::Terrain, &props) ){
            return -EINVAL;
        } 

        *out = props;
        return 0;
    }
    
    int SceneBlueprint::decodeObject( const char* data,
                                      int * idx, 
                                      boost::any* out )
    {
        Object temp;

        if( ObjectFactory::decodeObject(data, idx, &temp) ){
            return -EINVAL;
        }

        *out = temp;
        
        return 0;
    }

    int SceneBlueprint::decodeObjects( const char* data, 
                                       int * idx, 
                                       boost::any* out )

    {
        std::list<Object> temp;

        if( Ymir::decodeList<Object>(data, idx, &ObjectFactory::decodeObject, &temp) )
        {
            return -EINVAL;
        }   

        *out = temp;
        return 0;
    }

    /*void SceneBlueprint::setFog( Ogre::SceneManager* scene, boost::any& fog){
        scene->setFog();
    }*/

    void SceneBlueprint::setAmbient(Ogre::SceneManager* scene, boost::any& val){
        scene->setAmbientLight(boost::any_cast<Ogre::ColourValue>(val));
    }
}