#include "Object.h"

using namespace std;
using namespace boost;

namespace Ymir {
 
    /*void Object::setProperty(const std::string& name, boost::any& val){
        PropList::iterator it = props.begin();
    
        //If the key already exists, replace it
        if( (it = props.find(name)) != props.end() ){
           props.erase(name); 
        } 
    
        props.insert(std::pair<std::string, boost::any>(name, val));
    }
    
    bool Object::hasProperty(const std::string& name, boost::any* val = NULL){
        bool hasProp = false;
        PropList::iterator it = props.begin();
        
        if( (it = props.find(name)) != props.end() ){
            hasProp = true;
            
            if( val ){
                *val = it->second;
            }
        }
    
        return hasProp;
    }*/

    /********************** Utility Function Definitions ************************/
    int Object::decodePropListBase( propDecodeFP fps[], 
                                    const char* data, 
                                    int* idx, 
                                    PropList* output ){
        int count = 0;

        if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }   
    
        //Walk the list of given properties
        for( int i = 0; i < count; i++ ){
            int arity = 0;
            std::string prop = "";
            boost::any val;
    
            //Every prop must be of the form {name:string, prop:varies}
            if( ei_decode_tuple_header(data, idx, &arity) ||
                (arity != 2) ||
                decodeString(data, idx, &prop) )
            {
                return -EINVAL;
            }
  
            for( int j = 0; fps[j]; j++ ){
                int rc = 0; 
                propDecodeFP fp = fps[j];

                rc = fp(prop, data, idx, &val);

                if( rc == 0 ){
                    output->insert(std::pair<std::string, boost::any>(prop, val));
                    break;
                } 
            }
        }
    
        //Decode end of list
        if( count && ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }

        return 0;
    }
    
    int Object::decodeBool( const char* data, int* idx, int* output ){
        return ei_decode_boolean(data, idx, output);
    }
    
    int Object::decodeString( const char* data, int* idx, string* output ){
        int rc = 0;
        char temp [256] = {0};
        
        if( !(rc = ei_decode_string(data, idx, temp)) ){
            *output = string(temp);
        }
    
        return rc;
    }
    
    int Object::decodeFloat( const char* data, int* idx, float* output ){
        int rc = 0;
        double temp = 0;
        
        rc = ei_decode_double(data, idx, &temp);
    
        *output = ((float)temp);
    
        return rc;
    }
    
    int Object::decodeType( const char* data, int* idx, Ymir::Object::Type* output ){
        int rc = 0;
        string type = "";
    
        if( !(rc = decodeString(data, idx, &type)) ){
            
            if( type == "camera" ){
                *output = Ymir::Object::Camera;
            } else if( type == "light" ){
                *output = Ymir::Object::Light;
            } else if( type == "entity" ){
                *output = Ymir::Object::Entity;
            } else if( type == "window" ){
                *output = Ymir::Object::Window;
            } else if( type == "button" ){
                *output = Ymir::Object::Button;   
            } else {
                *output = Ymir::Object::Invalid;
            }
        }
    
        return rc;
    }
}


