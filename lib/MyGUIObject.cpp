#include <errno.h>

#include "MyGUIObject.h"

using namespace std;
using namespace boost;
using namespace MyGUI;

namespace Ymir {
   int Window::decodeProp( const std::string& prop, const char* data, int* idx ){
        int rc = 0;
        int bval;
        MyGUI::IntSize intSize;
    
        if( prop == "minSize" && !decodeIntSize(data, idx, &intSize) ){
            props.insert(std::pair<std::string, boost::any>(prop, intSize));
        } else if( prop == "maxSize" && !decodeIntSize(data, idx, &intSize) ){
            props.insert(std::pair<std::string, boost::any>(prop, intSize));
        } else if( prop == "snap" && !decodeBool(data, idx, &bval) ){
            props.insert(std::pair<std::string, boost::any>(prop, bval));
        } else {
            rc = -EINVAL;
        }
    
        return rc;
    }
   
   int Window::set( MyGUI::Window* window ){
        boost::any temp;

        if( hasProperty("minSize", &temp) ){
            window->setMinSize(boost::any_cast<MyGUI::IntSize>(temp));
        }
      
        if( hasProperty("maxSize", &temp) ){
            window->setMaxSize(boost::any_cast<MyGUI::IntSize>(temp));
        } 

        if( hasProperty("snap", &temp) ){
            window->setSnap(boost::any_cast<bool>(temp));
        }

        return 0;
    }

/********************** Button Definitions ************************/
    int Button::decodeProp(const std::string& prop, const char* args, int* idx){
        int pressed = false;

        if( prop == "selected" && decodeBool(args, idx, &pressed) ){
           props.insert(std::pair<std::string, boost::any>(prop, (bool)pressed));  
        }

        return 0;
    }

    int Button::set( MyGUI::Button* button ){
        boost::any temp;

        if( hasProperty("selected", &temp) ){
            button->setStateSelected(boost::any_cast<bool>(temp));
        }

        return 0;
    }

/********************** Utility Function Definitions ************************/
    int decodeIntCoord( const char* data, int* idx, MyGUI::IntCoord *output ){
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
    
    int decodeIntSize( const char* data, int* idx, MyGUI::IntSize *output ){
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
    
    int decodeUString( const char* data, int* idx, MyGUI::UString* output ){
        std::string temp = "";
    
        if( decodeString(data, idx, &temp) ){
            return -EINVAL;
        }
    
        *output = MyGUI::UString(temp);
    
        return 0;
    }
    
    int decodeAlign( const char* data, int* idx, MyGUI::Align* output ){
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

}
