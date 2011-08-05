#include "YmirObject.h"

using namespace std;
using namespace boost;

namespace Ymir {
    YmirObject::YmirObject( const std::string& uuid ) :
        uuid(uuid),
        props()
    {
    
    }
    
    YmirObject::~YmirObject(){
    
    }
    
    void YmirObject::setProperty(const string& name, any& val){
        PropList::iterator it = props.begin();
    
        //If the key already exists, replace it
        if( (it = props.find(name)) != props.end() ){
           props.erase(name); 
        } 
    
        props.insert(pair<string, any>(name, val));
    }
    
    
    bool YmirObject::hasProperty(const string& name, any* val = NULL){
        bool hasProp = false;
        PropList::iterator it = props.begin();
        
        if( (it = props.find(name)) != props.end() ){
            hasProp = true;
            
            if( val ){
                *val = it->second;
            }
        }
    
        return hasProp;
    }

    /********************** Utility Function Definitions ************************/
    int decodeBool( const char* data, int* idx, int* output ){
        return ei_decode_boolean(data, idx, output);
    }
    
    int decodeString( const char* data, int* idx, string* output ){
        int rc = 0;
        char temp [256] = {0};
        
        if( !(rc = ei_decode_string(data, idx, temp)) ){
            *output = string(temp);
        }
    
        return rc;
    }
    
    int decodeFloat( const char* data, int* idx, float* output ){
        int rc = 0;
        double temp = 0;
        
        rc = ei_decode_double(data, idx, &temp);
    
        *output = ((float)temp);
    
        return rc;
    }
    
    int decodeType( const char* data, int* idx, ObjectType* output ){
        int rc = 0;
        string type = "";
    
        if( !(rc = decodeString(data, idx, &type)) ){
            
            if( type == "camera" ){
                *output = OBJECT_CAMERA;
            } else if( type == "light" ){
                *output = OBJECT_LIGHT;
            } else if( type == "entity" ){
                *output = OBJECT_ENTITY;
            } else if( type == "window" ){
                *output = OBJECT_WINDOW;
            } else if( type == "button" ){
                *output = OBJECT_BUTTON;   
            } else {
                *output = OBJECT_INVALID;
            }
        }
    
        return rc;
    }
}


