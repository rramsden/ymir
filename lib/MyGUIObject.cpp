#include <errno.h>

#include "MyGUIObject.h"

#include "EventManager.h"
#include "Core.h"

using namespace std;
using namespace boost;
using namespace MyGUI;

namespace Ymir {

    int MyGUIObject::decodeProperty( const std::string& prop, 
                                     const char* data, 
                                     int* idx,
                                     boost::any* output ) 
    {
        int visible = 0;
        std::string sval = "";
        MyGUI::Colour color;
        MyGUI::IntCoord intCoord;
        MyGUI::Align align;
   
        if( prop == "visible" && !decodeBool(data, idx, &visible) ){
            *output = visible;
        }  else if( prop == "position" && !decodeIntCoord(data, idx, &intCoord) ){
            *output = intCoord;
        }  else if( prop == "skin" && !decodeString(data, idx, &sval) ){
            *output = sval;
        } else if( prop == "align" && !decodeAlign(data, idx, &align) ){
            *output = align;
        } else if( prop == "layer" && !decodeString(data, idx, &sval) ){
            *output = sval;
        } else if( prop == "caption" && !decodeString(data, idx, &sval) ){
            *output = sval;
        } else if( prop == "font" && !decodeString(data, idx, &sval) ){
            *output = sval;
        } else if( prop == "textAlign" && !decodeAlign(data, idx, &align) ){
            *output = align;
        } else {
            return -EINVAL;
        }

        return 0;
    }
 
    void MyGUIObject::setWidget( MyGUI::Widget* widget, Ymir::PropList& props ){
        /*boost::any temp;

        //Set common properties shared by MyGUI widgets
        if( this->hasProperty("caption", &temp) ){
            widget->setCaption(boost::any_cast<std::string>(temp));  
        }*/

        this->set(widget, props); 

        widget->setVisible(true);
    }

    void MyGUIObject::create( Ymir::PropList& props ) {
        MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();
        MyGUI::IntCoord intCoord; 
        std::string skin, layer;
        MyGUI::Align align;
   
        //Required properties for all MyGUI objects 
        if( !gui ||
            !props.hasProperty<MyGUI::IntCoord>("position", &intCoord) ||
            !props.hasProperty<std::string>("skin", &skin) ||
            !props.hasProperty<MyGUI::Align>("align", &align) ||
            !props.hasProperty<std::string>("layer", &layer) )
        {
            Ymir::Core::getSingletonPtr()->logNormal("Not enough data to create MyGUI Object!"); 
            return;
        }

        Ymir::Core::getSingletonPtr()->logNormal("Creating MyGUI Object: " 
                                                  "Skin: " + skin + ", Layer: " + layer + ", Name: " + uuid);

        MyGUI::Widget* widget = this->create( skin,
                                              intCoord, 
                                              align,
                                              layer,
                                              uuid,
                                              gui);

        this->set(widget, props);
    
        //Lastly, inform EventManager about the new GUI object
        EventManager::getSingletonPtr()->monitor(widget);
    }

    void MyGUIObject::update( Ymir::PropList& actions ){
        MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();

        this->set( gui->findWidgetT(uuid), actions );
    }

    void MyGUIObject::destroy(){
        MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr(); 

        gui->destroyChildWidget( gui->findWidgetT(uuid) );
    }

    int MyGUIObject::decodeIntCoord( const char* data, int* idx, MyGUI::IntCoord *output ){
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
    
        *output = MyGUI::IntCoord((int)left, (int)top, (int)width, (int)height);
    
        return 0;
    }
    
    int MyGUIObject::decodeIntSize( const char* data, int* idx, MyGUI::IntSize *output ){
        int arity = 0;
        long width, height;
    
        if( ei_decode_tuple_header(data, idx, &arity) ||
            (arity != 2) ||
            ei_decode_long(data, idx, &width) ||
            ei_decode_long(data, idx, &height) )
        {
            return -EINVAL;
        }
    
        *output = MyGUI::IntSize((int)width, (int)height);
    
        return 0;
    }
    
    int MyGUIObject::decodeUString( const char* data, int* idx, MyGUI::UString* output ){
        std::string temp = "";
    
        if( decodeString(data, idx, &temp) ){
            return -EINVAL;
        }
    
        *output = MyGUI::UString(temp);
    
        return 0;
    }
    
    int MyGUIObject::decodeAlign( const char* data, int* idx, MyGUI::Align* output ){
        std::string temp = "";


        if( decodeString(data, idx, &temp) ){
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

/********************** Window Definitions ************************/
   MyGUI::Widget* Window::create( std::string& skin, 
                                  MyGUI::IntCoord& coord, 
                                  MyGUI::Align& align,
                                  std::string& layer,
                                  std::string& name,
                                  MyGUI::Gui* gui )
    {
        MyGUI::Widget* widget = gui->createWidget<MyGUI::Window>(skin, coord,
                                                                 align, layer,
                                                                 name);

        ptr = widget;

        return widget;
    }
  
   int Window::decodeProperty( const std::string& prop, 
                               const char* data, 
                               int* idx,
                               boost::any* output ){
        int rc = 0;
        int bval;
        MyGUI::IntSize intSize;
    
        if( prop == "minSize" && !decodeIntSize(data, idx, &intSize) ){
            *output = intSize;
        } else if( prop == "maxSize" && !decodeIntSize(data, idx, &intSize) ){
            *output = intSize;
        } else if( prop == "snap" && !decodeBool(data, idx, &bval) ){
            *output = bval;
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
   
   int Window::decodePropList( const char* args,
                               int* idx,
                               Ymir::PropList* output )
   {
        propDecodeFP fps[] = { Ymir::MyGUIObject::decodeProperty,
                               Ymir::Window::decodeProperty,
                               NULL }; 

        return Ymir::Object::decodePropListBase(fps, args, idx, output); 
   }

   void Window::set( MyGUI::Widget* widget, Ymir::PropList& props ){
        MyGUI::Window* window = static_cast<MyGUI::Window*>(widget);
        boost::any temp;

        if( props.hasProperty("minSize", &temp) ){
            window->setMinSize(boost::any_cast<MyGUI::IntSize>(temp));
        }
      
        if( props.hasProperty("maxSize", &temp) ){
            window->setMaxSize(boost::any_cast<MyGUI::IntSize>(temp));
        } 

        if( props.hasProperty("snap", &temp) ){
            window->setSnap(boost::any_cast<bool>(temp));
        }
    }

/********************** Button Definitions ************************/

    MyGUI::Widget* Button::create( std::string& skin, 
                                   MyGUI::IntCoord& coord, 
                                   MyGUI::Align& align,
                                   std::string& layer,
                                   std::string& name,
                                   MyGUI::Gui* gui )
    {
       
        Ymir::Core::getSingletonPtr()->logNormal("Creating button with name = " + name);

        MyGUI::Widget* widget =  gui->createWidget<MyGUI::Button>(skin, coord,
                                                                  align, layer,
                                                                  name);
        ptr = widget;

        return widget;
    }
 
    int Button::decodePropList( const char* args,
                                int* idx,
                                Ymir::PropList* output )
    {
        propDecodeFP fps[] = { Ymir::MyGUIObject::decodeProperty,
                               Ymir::Button::decodeProperty,
                               NULL }; 

        return Ymir::Object::decodePropListBase(fps, args, idx, output); 
    }

    int Button::decodeProperty( const std::string& prop, 
                                const char* args, 
                                int* idx, 
                                boost::any* output ){
        int rc = 0;
        int pressed = false;

        if( prop == "selected" && !decodeBool(args, idx, &pressed) ){
            *output = (bool)pressed;
        } else {
            rc = -EINVAL;
        }

        return rc;
    }

    void Button::set( MyGUI::Widget* widget, Ymir::PropList& props ){
        MyGUI::Button* button = static_cast<MyGUI::Button*>(widget);
        boost::any temp;

        if( props.hasProperty("caption", &temp) ){
            button->setCaption(boost::any_cast<string>(temp));
        } else if( props.hasProperty("selected", &temp) ){
            button->setButtonPressed(boost::any_cast<bool>(temp));
        } else if( props.hasProperty("visible", &temp) ){
            button->setVisible(boost::any_cast<int>(temp));
        }
    }
}
