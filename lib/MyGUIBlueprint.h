#ifndef _MYGUIBLUEPRINT_H
#define _MYGUIBLUEPRINT_H

#include <MyGUI.h>

#include "ObjectBlueprint.h"
#include "EventManager.h"
#include "Core.h"

namespace Ymir {

    template <class T>
    class MyGUIBlueprint : public ObjectBlueprint {

        public:

            void create( std::string& id, PropList& props ){
                Ymir::Core* core = Ymir::Core::getSingletonPtr();
                MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();
                EventManager* em = EventManager::getSingletonPtr();
                MyGUI::IntCoord intCoord; 
                std::string skin, layer;
                MyGUI::Align align;
                
                if( !props.hasProperty<MyGUI::IntCoord>("position", &intCoord) ||
                    !props.hasProperty<std::string>("skin", &skin) ||
                    !props.hasProperty<MyGUI::Align>("align", &align) ||
                    !props.hasProperty<std::string>("layer", &layer) ) 
                {
                    //<<HERE>> TODO: Throw exception
                    core->logNormal("Not enough data to create MyGUI Object!");
                    return;
                }

                core->logNormal("Creating MyGUI Object: " 
                                "Skin: " + skin + ", "
                                "Layer: " + layer + ", Name: " + id); 

                
                T* obj = gui->createWidget<T>(skin, intCoord, align, layer, id);

                set(obj, props);

                //Inform EventManager about the new GUI object
                em->monitor(obj);
            }

            void update( std::string& id, PropList& props ){
                MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();

                T* obj = gui->findWidget<T>(id);

                set(obj, props);
            } 

            void destroy( std::string& id, PropList& props ){
                MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();

                gui->destroyWidget( gui->findWidget<T>(id) );
            }
           
            static int decodeIntCoord( const char* data, 
                                       int* idx, 
                                       boost::any* output )
            {
                int arity = 0;
                long left, top, width, height;
    
                if( ei_decode_tuple_header(data, idx, &arity) ||
                    (arity != 4) ||
                    ei_decode_long(data, idx, &left) ||
                    ei_decode_long(data, idx, &top) ||
                    ei_decode_long(data, idx, &width) ||
                    ei_decode_long(data, idx, &height) )
                {
                    return -EINVAL;
                }

                *output = MyGUI::IntCoord( (int)left, 
                                           (int)top, 
                                           (int)width, 
                                           (int)height );
    
                return 0;
            }
       
            static int decodeAlign( const char* data, 
                                    int* idx, 
                                    boost::any* output )
            {
                std::string temp = "";


                if( Ymir::decodeString(data, idx, &temp) ){
                    return -EINVAL;
                }

                if( temp == "HCenter" ){
                    *output = MyGUI::Align(MyGUI::Align::HCenter);
                } else if( temp == "VCenter" ){
                    *output = MyGUI::Align(MyGUI::Align::VCenter);
                } else if( temp == "Left" ){
                    *output = MyGUI::Align(MyGUI::Align::Left);
                } else if( temp == "Right" ){
                    *output = MyGUI::Align(MyGUI::Align::Right);
                } else if( temp == "HStretch" ){
                    *output = MyGUI::Align(MyGUI::Align::HStretch);
                } else if( temp == "Top" ){
                    *output = MyGUI::Align(MyGUI::Align::Top);
                } else if( temp == "Bottom" ){
                    *output = MyGUI::Align(MyGUI::Align::Bottom);
                } else if( temp == "VStretch" ){
                    *output = MyGUI::Align(MyGUI::Align::VStretch);
                } else if( temp == "Stretch" ){
                    *output = MyGUI::Align(MyGUI::Align::Stretch);
                } else {
                    *output = MyGUI::Align(MyGUI::Align::Default);
                }

                return 0;
            }

            static void setVisible( T* obj, boost::any& val ){
               Ymir::Core* core = Ymir::Core::getSingletonPtr();
                
                core->logNormal("Setting visible!!!!!");

                obj->setVisible(boost::any_cast<bool>(val));
            }

            static void setCaption( T* obj, boost::any& val){
                obj->setCaption(boost::any_cast<std::string>(val));
            }

        protected:

            MyGUIBlueprint(){
                mBlueprint.insert(
                    BPEntry("position", BPFP(&decodeIntCoord, NULL)) );

                mBlueprint.insert(
                    BPEntry("skin", BPFP(&decodeString, NULL)) );

                mBlueprint.insert(
                    BPEntry("align", BPFP(&decodeAlign, NULL)) );

                mBlueprint.insert(
                    BPEntry("layer", BPFP(&decodeString, NULL)) );
            
                mBlueprint.insert(
                    BPEntry("visible", BPFP(&decodeBool, (setFP)&setVisible)) );

                mBlueprint.insert(
                    BPEntry("caption", BPFP(&decodeString, (setFP)&setCaption)) );
            
            }

    };
}
#endif
