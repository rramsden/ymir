#ifndef _NODEBLUEPRINT
#define _NODEBLUEPRINT

#include <LinearMath/btMotionState.h>

#include "Core.h"
#include "OgreBlueprint.h"

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
    class NodeBlueprint : public OgreBlueprint, public btMotionState {

        public:

            virtual void create( std::string& id, Ymir::PropList& props ){
                boost::any temp;
                SceneManager* scene = Ymir::Core::getSingletonPtr()->mScene;

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
                SceneManager* scene = Ymir::Core::getSingletonPtr()->mScene;

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
                SceneManager* scene = Ymir::Core::getSingletonPtr()->mScene;

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

            static void setScale(NodeTuple<T>* t, boost::any& scale){
                t->mNode->setScale(boost::any_cast<Vector3>(scale));
            }

            static void setOrientation(NodeTuple<T>* t, boost::any& orient){
                Ogre::Vector4 temp = boost::any_cast<Ogre::Vector4>(orient);

                t->mNode->setOrientation(temp.w, temp.x, temp.y, temp.z);
            }


            //Bullet physics interface
            virtual void getWorldTransform(btTransform& worldTrans) const{}
            virtual void setWorldTransform(const btTransform& worldTrans){}

        protected:

            NodeBlueprint() : Ymir::OgreBlueprint() {

                mBlueprint["position"] = BPFP(&decodeVector3, (setFP)&setPosition);
                mBlueprint["move"] = BPFP(&decodeVector3, (setFP)&setMove);
                mBlueprint["direction"] = BPFP(&decodeVector3, (setFP)&setDirection);
                mBlueprint["yaw"] = BPFP(&decodeRadian, (setFP)&setYaw);
                mBlueprint["pitch"] = BPFP(&decodeRadian, (setFP)&setPitch);
                mBlueprint["roll"] = BPFP(&decodeRadian, (setFP)&setRoll);
                mBlueprint["lookAt"] = BPFP(&decodeVector3, (setFP)&setLookAt);
                mBlueprint["scale"] = BPFP(&decodeVector3, (setFP)&setScale);
                mBlueprint["orientation"] = BPFP(&decodeVector4, (setFP)&setOrientation);
            }

           ~NodeBlueprint(){} 

            virtual T* createOgreObject( std::string&, Ymir::PropList&, SceneManager* ) = 0;
            virtual T* findOgreObject( std::string&, SceneManager* ) = 0;
            virtual void destroyOgreObject( std::string&, SceneManager* ) = 0;
    };
}
#endif
