#ifndef MYGUIOBJECT_H
#define MYGUIOBJECT_H

#include <errno.h>

//MyGUI
#include <MyGUI.h>
#include <MyGUI_Types.h>

#include "YmirObject.h"

namespace Ymir {

    int decodeIntCoord( const char* data, int* idx, MyGUI::IntCoord* output );
    int decodeIntSize( const char* data, int* idx, MyGUI::IntSize* output );
    int decodeUString( const char* data, int* idx, MyGUI::UString* output );
    int decodeAlign( const char* data, int* idx, MyGUI::Align* output );

    template<typename T>
    class MyGUIObject : public YmirObject {
       
        public:
            MyGUIObject( const std::string& uuid,
                         const char* args, 
                         int* idx );
            ~MyGUIObject() {};
    
            int decodeProps( const char* args, int* idx );
    
            virtual int decodeProp( const std::string& prop, const char* args, int* idx ) = 0;
    
            T* add( MyGUI::Gui* gui );
            T* update( MyGUI::Gui* gui );

        protected:
            virtual int set( T* object ) = 0;
    };
    
    class Window : public MyGUIObject<MyGUI::Window> {
    
        public:
            Window( const std::string& uuid,
                    const char* args,
                    int* idx ) : MyGUIObject<MyGUI::Window>(uuid, args, idx) {};
            ~Window(){};

            int decodeProp( const std::string& prop, const char* args, int* idx );
   
        protected:
            int set( MyGUI::Window* window );
    };
 
    class Button : public MyGUIObject<MyGUI::Button> {

        public:
            Button( const std::string& uuid,
                    const char* args, 
                    int* idx ) : MyGUIObject<MyGUI::Button>(uuid, args, idx) {};
            ~Button(){};

            int decodeProp( const std::string& prop, const char* args, int* idx );
            
        protected:
           int set( MyGUI::Button* button );
    };

    /* 
    class Button : public MyGUIObject<MyGUI::Button> {
    
        public:
            int decodeAddProp( const std::string& prop, char* args, int* idx );
            int decodeUpdateAction( const std::string& prop, char* args, int* idx );
    
            int add( MyGUI::Gui* gui );
            int update( MyGUI::Gui* gui );
    };*/

    template<typename T>
    MyGUIObject<T>::MyGUIObject( const std::string& uuid,
                                 const char* args,
                                 int* idx ) : YmirObject(uuid)
    {
        this->decodeProps(args, idx);
    }

    template<typename T>
    int MyGUIObject<T>::decodeProps( const char* data, int* idx ){
        int count = 0;
    
        if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }   
    
        //Walk the list of given properties
        for( int i = 0; i < count; i++ ){
            int arity = 0;
            std::string sval = "", prop = "";
            MyGUI::Colour color;
            MyGUI::IntCoord intCoord;
            MyGUI::Align align;
    
            //Every prop must be of the form {name:string, prop:varies}
            if( ei_decode_tuple_header(data, idx, &arity) ||
                (arity != 2) ||
                decodeString(data, idx, &prop) )
            {
                return -EINVAL;
            }
    
            if( prop == "position" && !decodeIntCoord(data, idx, &intCoord) ){
                props.insert(std::pair<std::string, boost::any>(prop,intCoord));  
            }  else if( prop == "skin" && !decodeString(data, idx, &sval) ){
                props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "align" && !decodeAlign(data, idx, &align) ){
                props.insert(std::pair<std::string, boost::any>(prop, align));
            } else if( prop == "layer" && !decodeString(data, idx, &sval) ){
                props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "caption" && !decodeString(data, idx, &sval) ){
                props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "font" && !decodeString(data, idx, &sval) ){
                props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "textAlign" && !decodeAlign(data, idx, &align) ){
                props.insert(std::pair<std::string, boost::any>(prop, align)); 
            }
            /*else if( prop == "textColor" && !decodeColor(data, idx, &color) ){
                props.insert(std::pair<std::string, boost:any>(prop, color));
            }*/
            
            else if( this->decodeProp(prop, data, idx) ){
                return -EINVAL;
            }
        }
    
        //Decode end of list
        if( ei_decode_list_header(data, idx, &count) || count ){
            return -EINVAL;
        }
    
        return 0;
    }
        
    template<typename T>
    T* MyGUIObject<T>::add( MyGUI::Gui* gui  ){
        T* widget = NULL;
        MyGUI::IntCoord intCoord; 
        std::string skin, layer;
        MyGUI::Align align;
        boost::any t1, t2, t3, t4, temp; 
    
        if( !gui ||
            !hasProperty("position", &t1) ||
            !hasProperty("skin", &t2) ||
            !hasProperty("align", &t3) ||
            !hasProperty("layer", &t4) )
        {
            return NULL;
        }
    
        intCoord = boost::any_cast<MyGUI::IntCoord>(t1);
        skin = boost::any_cast<std::string>(t2);
        align = boost::any_cast<MyGUI::Align>(t3);
        layer = boost::any_cast<std::string>(t4);
 
        widget = gui->createWidget<T>(skin, intCoord, align, layer, uuid);

        //Set common properties shared by MyGUI widgets
        if( hasProperty("caption", &temp) ){
            widget->setCaption(boost::any_cast<std::string>(temp));  
        }

        this->set(widget);

        return widget;
    }
    
    template<typename T>
    T* MyGUIObject<T>::update( MyGUI::Gui* gui ){
        return NULL; 
    }
   
}
#endif
