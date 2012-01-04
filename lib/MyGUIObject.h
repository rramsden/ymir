#ifndef MYGUIOBJECT_H
#define MYGUIOBJECT_H

#include <errno.h>

//MyGUI
#include <MyGUI.h>

#include "Object.h"

namespace Ymir {


    class MyGUIObject : public Ymir::Object {
       
        public:
            MyGUIObject( const std::string& uuid,
                         Ymir::Object::Type type ) : Ymir::Object(uuid, type) {}
            
            ~MyGUIObject() {};
   
            void create( Ymir::PropList& props );
            void update( Ymir::PropList& actions );
            void destroy();

            static int decodeIntCoord( const char* data, int* idx, MyGUI::IntCoord* output );
            static int decodeIntSize( const char* data, int* idx, MyGUI::IntSize* output );
            static int decodeUString( const char* data, int* idx, MyGUI::UString* output );
            static int decodeAlign( const char* data, int* idx, MyGUI::Align* output );

        protected:
            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx,
                                       boost::any* output );

            virtual MyGUI::Widget* create( std::string& skin, 
                                           MyGUI::IntCoord& coord, 
                                           MyGUI::Align& align,
                                           std::string& layer,
                                           std::string& name,
                                           MyGUI::Gui* gui ) = 0;
            
            void setWidget( MyGUI::Widget* widget, Ymir::PropList& props );
            virtual void set( MyGUI::Widget* widget, Ymir::PropList& props ) = 0;
    };
    
    class Window : public MyGUIObject {
    
        public:
            Window( const std::string& uuid ) : Ymir::MyGUIObject(uuid, Ymir::Object::Window) {}
            ~Window(){}

            static int decodePropList( const char* args, 
                                       int* idx,
                                       Ymir::PropList* output );

        protected:
           static int decodeProperty( const std::string& prop, 
                                      const char* data, 
                                      int* idx,
                                      boost::any* output );

            MyGUI::Widget* create( std::string& skin, 
                                   MyGUI::IntCoord& coord, 
                                   MyGUI::Align& align,
                                   std::string& layer,
                                   std::string& name,
                                   MyGUI::Gui* gui );

            void set( MyGUI::Widget* widget, Ymir::PropList& props );
    };
 
    class Button : public Ymir::MyGUIObject {

        public:
            Button( const std::string& uuid ) : Ymir::MyGUIObject(uuid, Ymir::Object::Button) {}
            ~Button(){}

            static int decodePropList( const char* args, 
                                       int* idx,
                                       Ymir::PropList* output );

        protected:

            static int decodeProperty( const std::string& prop, 
                                       const char* args, 
                                       int* idx, 
                                       boost::any* output );

            MyGUI::Widget* create( std::string& skin, 
                                   MyGUI::IntCoord& coord, 
                                   MyGUI::Align& align,
                                   std::string& layer,
                                   std::string& name,
                                   MyGUI::Gui* gui );

            void set( MyGUI::Widget* widget, Ymir::PropList& props );
    };

    /* 
    class Button : public MyGUIObject<MyGUI::Button> {
    
        public:
            int decodeAddProp( const std::string& prop, char* args, int* idx );
            int decodeUpdateAction( const std::string& prop, char* args, int* idx );
    
            int add( MyGUI::Gui* gui );
            int update( MyGUI::Gui* gui );
    };*/

    /*template<typename T>
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
                this->props.insert(std::pair<std::string, boost::any>(prop,intCoord));  
            }  else if( prop == "skin" && !decodeString(data, idx, &sval) ){
                this->props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "align" && !decodeAlign(data, idx, &align) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, align));
            } else if( prop == "layer" && !decodeString(data, idx, &sval) ){
                this->props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "caption" && !decodeString(data, idx, &sval) ){
                this->props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "font" && !decodeString(data, idx, &sval) ){
                this->props.insert(std::pair<std::string, boost::any>(prop,sval));
            } else if( prop == "textAlign" && !decodeAlign(data, idx, &align) ){
                this->props.insert(std::pair<std::string, boost::any>(prop, align)); 
            }
            else if( prop == "textColor" && !decodeColor(data, idx, &color) ){
                props.insert(std::pair<std::string, boost:any>(prop, color));
            }
            
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
            !this->hasProperty("position", &t1) ||
            !this->hasProperty("skin", &t2) ||
            !this->hasProperty("align", &t3) ||
            !this->hasProperty("layer", &t4) )
        {
            return NULL;
        }
    
        intCoord = boost::any_cast<MyGUI::IntCoord>(t1);
        skin = boost::any_cast<std::string>(t2);
        align = boost::any_cast<MyGUI::Align>(t3);
        layer = boost::any_cast<std::string>(t4);
 
        widget = gui->createWidget<T>(skin, intCoord, align, layer, this->uuid);

        //Set common properties shared by MyGUI widgets
        if( this->hasProperty("caption", &temp) ){
            widget->setCaption(boost::any_cast<std::string>(temp));  
        }

        this->set(widget);

        

        return widget;
    }
    
    template<typename T>
    T* MyGUIObject<T>::update( MyGUI::Gui* gui ){
        return NULL; 
    }

    template<typename T>
    void MyGUIObject<T>::destroy( MyGUI::Gui* gui ){
        MyGUI::Widget* widget = gui->findWidgetT(this->uuid);

        gui->destroyChildWidget(widget);    
    }*/

}
#endif
