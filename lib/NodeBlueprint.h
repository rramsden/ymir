#ifndef _NODEBLUEPRINT
#define _NODEBLUEPRINT

#include "OgreBlueprint.h"
#include "Core.h"

using namespace Ogre;

namespace Ymir {

    template<class T>
    class NodeTuple {

        public:
            NodeTuple(SceneNode* node, T* obj) : mNode(node), mObject(obj) {}
            ~NodeTuple(){}

            SceneNode* mNode;
            T* mObject;
    };
    
    template<class T>
    class NodeBlueprint : public OgreBlueprint {

        public:

            virtual void create( std::string& id, Ymir::PropList& props ){
                boost::any temp;
                SceneManager* scene = 
                    Ymir::Core::getSingletonPtr()->getActiveScene();

                //If a target scene is given, use it instead of the default
                if( props.hasProperty("scene", &temp) ){
                    std::string sceneID = boost::any_cast<std::string>(temp);
                    scene = Ymir::Core::getSingletonPtr()->findScene(sceneID);
                }

                //Create the actual object
                T* obj = createOgreObject(id, props, scene); 
                if( !obj ){
                    //<<HERE>> TODO: Throw exception
                }

                SceneNode* node=scene->getRootSceneNode()->createChildSceneNode();

                node->attachObject(obj);

                //Call proper set function pointers 
                NodeTuple<T> tuple(node, obj);
                set(&tuple, props);
            }

            virtual void update( std::string& id, Ymir::PropList& props ){
                boost::any temp;
                SceneManager* scene = 
                    Ymir::Core::getSingletonPtr()->getActiveScene();

                //If a target scene is given, use it instead of the default
                if( props.hasProperty("scene", &temp) ){
                    std::string sceneID = boost::any_cast<std::string>(temp);
                    scene = Ymir::Core::getSingletonPtr()->findScene(sceneID);
                }
               
                T* obj = findOgreObject(id, scene);
                if( !obj ){
                    //<<HERE>> TODO: Throw exception
                }
                
                SceneNode* node = obj->getParentSceneNode();

                NodeTuple<T> tuple(node, obj);
                set(&tuple, props);
            }
            
            virtual void destroy( std::string& id, PropList& props ){
                boost::any temp;
                SceneManager* scene = 
                    Ymir::Core::getSingletonPtr()->getActiveScene();

                //If a target scene is given, use it instead of the default
                if( props.hasProperty("scene", &temp) ){
                    std::string sceneID = boost::any_cast<std::string>(temp);
                    scene = Ymir::Core::getSingletonPtr()->findScene(sceneID);
                }

                destroyOgreObject(id, scene);
            }

            //Setters
            static void setPosition(NodeTuple<T>* t, boost::any& pos){
                t->mNode->setPosition(boost::any_cast<Vector3>(pos));
            }

            static void setMove(NodeTuple<T>* t, boost::any& diff){
                t->mNode->translate( t->mNode->getOrientation() * 
                                     boost::any_cast<Ogre::Vector3>(diff) );
            }


            static void setDirection(NodeTuple<T>* t, boost::any& dir){
                t->mNode->setDirection(boost::any_cast<Vector3>(dir));    
            }

            static void setYaw(NodeTuple<T>* t, boost::any& rad){
                t->mNode->yaw(boost::any_cast<Radian>(rad));
            }


            static void setPitch(NodeTuple<T>* t, boost::any& rad){
                t->mNode->pitch(boost::any_cast<Radian>(rad));
            }


            static void setRoll(NodeTuple<T>* t, boost::any& rad){
                t->mNode->roll(boost::any_cast<Radian>(rad));
            }

            static void setLookAt(NodeTuple<T>* t, boost::any& look){
                t->mNode->lookAt(boost::any_cast<Vector3>(look), Ogre::Node::TS_WORLD);
            }

        protected:

            NodeBlueprint() : Ymir::OgreBlueprint() {

                mBlueprint.insert( 
                    BPEntry("position", BPFP(&decodeVector3, (setFP)&setPosition)) );

                mBlueprint.insert(
                    BPEntry("move", BPFP(&decodeVector3, (setFP)&setMove)));

                mBlueprint.insert(
                    BPEntry("direction", BPFP(&decodeVector3, (setFP)&setDirection)) );
                
                mBlueprint.insert(
                    BPEntry("yaw", BPFP(&decodeRadian, (setFP)&setYaw)) );

                mBlueprint.insert(
                    BPEntry("pitch", BPFP(&decodeRadian, (setFP)&setPitch)) );

                mBlueprint.insert(
                    BPEntry("roll", BPFP(&decodeRadian, (setFP)&setRoll)) );

                mBlueprint.insert(
                    BPEntry("lookAt", BPFP(&decodeVector3, (setFP)&setLookAt)) );
            }

           ~NodeBlueprint(){} 

            virtual T* createOgreObject( std::string&, Ymir::PropList&, SceneManager* ) = 0;
            virtual T* findOgreObject( std::string&, SceneManager* ) = 0;
            virtual void destroyOgreObject( std::string&, SceneManager* ) = 0;

    };
}
#endif
