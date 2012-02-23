#ifndef _NODEBLUEPRINT
#define _NODEBLUEPRINT

#include <LinearMath/btMotionState.h>

#include "Core.h"
#include "BtOgreGP.h"
#include "OgreBlueprint.h"

using namespace Ogre;

namespace Ymir {

    template<class T>
    class NodeInfo {

        public:
            NodeInfo(SceneNode* node, T* obj, btRigidBody* rbody) : 
                mNode(node), mObject(obj), mRigidBody(rbody) {}
            ~NodeInfo(){}

            SceneNode* mNode;
            T* mObject;
            btRigidBody* mRigidBody;
    };
    
    template<class T>
    class NodeBlueprint : public OgreBlueprint {

        public:

            virtual void create( std::string& id, Ymir::PropList& props ){
                Core* core = Core::getSingletonPtr();
                boost::any temp;
                SceneManager* scene = core->mScene;

                //Create the actual object
                T* obj = createOgreObject(id, props, scene); 
                if( !obj ){
                    //<<HERE>> TODO: Throw exception
                }

                SceneNode* node=scene->getRootSceneNode()->createChildSceneNode();

                node->attachObject(obj);

                //TODO: Run through object options and configure the object

                //Finally create the physics object from the positioned object
                btCollisionShape* shape = createPhysicsObject(obj);
                btRigidBody* body = NULL;
                btScalar mass = 0;
                btVector3 inertia;
               
                //Set mass properties
                props.hasProperty<float>("mass", &mass);
                shape->calculateLocalInertia(mass, inertia); 

                BtOgre::RigidBodyState* state = 
                    new BtOgre::RigidBodyState(node);

                body = new btRigidBody(mass, state, shape, inertia);
                
                core->mDynamicsWorld->addRigidBody(body);
            
                RigidObjectInfo info = {shape, body, state};
                core->mRigidObjects[id] = info; 

                //TODO: Attach child objects
                //Set the properties of the object
                NodeInfo<T> tuple(node, obj, body);
                set(&tuple, props);

                updatePhysics(&tuple, props);
            }

            virtual void update( std::string& id, Ymir::PropList& props ){
                boost::any temp;
                Core* core = Core::getSingletonPtr();
                SceneManager* scene = core->mScene;
                btRigidBody* body = core->mRigidObjects[id].body;

                T* obj = findOgreObject(id, scene);
                if( !obj ){
                    //<<HERE>> TODO: Throw exception
                }
                
                SceneNode* node = obj->getParentSceneNode();

                NodeInfo<T> tuple(node, obj, body);
                set(&tuple, props);

                updatePhysics(&tuple, props);
            }
            
            virtual void destroy( std::string& id, PropList& props ){
                boost::any temp;
                SceneManager* scene = Ymir::Core::getSingletonPtr()->mScene;

                destroyOgreObject(id, scene);
            }

            //Setters
            static void setPosition(NodeInfo<T>* t, boost::any& pos){
                Vector3 vPos = boost::any_cast<Vector3>(pos);

                t->mNode->setPosition(vPos);
            }

            static void setMove(NodeInfo<T>* t, boost::any& diff){
                Vector3 vDiff = boost::any_cast<Vector3>(diff);

                t->mNode->translate(t->mNode->getOrientation() * vDiff);
            }


            static void setDirection(NodeInfo<T>* t, boost::any& dir){
                t->mNode->setDirection(boost::any_cast<Vector3>(dir));    
            }

            static void setYaw(NodeInfo<T>* t, boost::any& rad){
                t->mNode->yaw(boost::any_cast<Radian>(rad));
            }


            static void setPitch(NodeInfo<T>* t, boost::any& rad){
                t->mNode->pitch(boost::any_cast<Radian>(rad));
            }


            static void setRoll(NodeInfo<T>* t, boost::any& rad){
                t->mNode->roll(boost::any_cast<Radian>(rad));
            }

            static void setLookAt(NodeInfo<T>* t, boost::any& look){
                t->mNode->lookAt(boost::any_cast<Vector3>(look), Ogre::Node::TS_WORLD);
            }

            static void setScale(NodeInfo<T>* t, boost::any& scale){
                Vector3 vScale = boost::any_cast<Vector3>(scale);
                
                t->mNode->setScale(vScale);
            }

            static void setOrientation(NodeInfo<T>* t, boost::any& orient){
                Ogre::Vector4 temp = boost::any_cast<Ogre::Vector4>(orient);

                t->mNode->setOrientation(temp.w, temp.x, temp.y, temp.z);
            }

            static void setMass(NodeInfo<T>* t, boost::any& gMass){
                
                btScalar mass = boost::any_cast<float>(gMass);
                btVector3 inertia(0,0,0);

                t->mRigidBody->getCollisionShape()
                    ->calculateLocalInertia(mass, inertia);



                t->mRigidBody->setMassProps(mass, inertia);
            }

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

                mBlueprint["mass"] = BPFP(&decodeFloat, NULL);
            }

           ~NodeBlueprint(){} 

            virtual T* createOgreObject( std::string&, Ymir::PropList&, SceneManager* ) = 0;
            virtual btCollisionShape* createPhysicsObject(T* obj) { 
                Core::getSingletonPtr()->logNormal("Creating empty shape!");
                return new btEmptyShape; }
            virtual T* findOgreObject( std::string&, SceneManager* ) = 0;
            virtual void destroyOgreObject( std::string&, SceneManager* ) = 0;


            void updatePhysics(NodeInfo<T>* i, PropList& props){
                SceneNode* node = i->mNode;
                Vector3 pos = node->getPosition();
                Vector3 scale = node->getScale();
                Quaternion orient = node->getOrientation();
                btScalar mass = 0;
                btVector3 inertia(0,0,0);
                btTransform trans;

                trans.setIdentity();
                trans.setOrigin(BtOgre::Convert::toBullet(pos)); 
                trans.setRotation(BtOgre::Convert::toBullet(orient));

                i->mRigidBody->getCollisionShape()->setLocalScaling(BtOgre::Convert::toBullet(scale));
            
                i->mRigidBody->setCenterOfMassTransform(trans);

                //Set mass properties
                //props.hasProperty<float>("mass", &mass);
                //i->mRigidBody->getCollisionShape()->calculateLocalInertia(mass, inertia);
                //i->mRigidBody->setMassProps(mass, inertia);
                //Core::getSingletonPtr()->logNormal("mass: " + Ogre::StringConverter::toString((float)mass) +
//                                                   ", inertia: " + Ogre::StringConverter::toString(BtOgre::Convert::toOgre(inertia)) );
            }
    };
}
#endif
