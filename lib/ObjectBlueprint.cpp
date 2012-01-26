#include "ObjectBlueprint.h"

using namespace Ymir;
using namespace std;
using namespace boost;

namespace Ymir {

    void ObjectBlueprint::set( void* ptr, PropList& props ){
        Blueprint::iterator it;

        for(it = mBlueprint.begin(); it != mBlueprint.end(); it++){
            boost::any temp;

            if( props.hasProperty(it->first, &temp) ){
                
                if( it->second.second ){
                    it->second.second(ptr, temp);
                }
            }
        }
    }

    int ObjectBlueprint::decodeBool( const char* data, int* idx, bool* output ){
        int temp = 0;

        if( !ei_decode_boolean(data, idx, &temp) ){
            *output = (bool)temp;
        } else {
            return -EINVAL;
        }

        return 0;
    }
    
    int ObjectBlueprint::decodeString( const char* data, int* idx, string* output ){
        int rc = 0;
        char temp [256] = {0};
        
        if( !(rc = ei_decode_string(data, idx, temp)) ){
            *output = string(temp);
        }
    
        return rc;
    }
    
    int ObjectBlueprint::decodeFloat( const char* data, int* idx, float* output ){
        int rc = 0;
        double temp = 0;
        
        rc = ei_decode_double(data, idx, &temp);
    
        *output = ((float)temp);
    
        return rc;
    }
    
    int ObjectBlueprint::decodeType( const char* data, int* idx, Ymir::ObjectType* output ){
        int rc = 0;
        string type = "";
    
        if( !(rc = decodeString(data, idx, &type)) ){
            
            if( type == "camera" ){
                *output = Ymir::Camera;
            } else if( type == "light" ){
                *output = Ymir::Light;
            } else if( type == "entity" ){
                *output = Ymir::Entity;
            } else if( type == "window" ){
                *output = Ymir::Window;
            } else if( type == "button" ){
                *output = Ymir::Button;   
            } else {
                *output = Ymir::Invalid;
            }
        }
    
        return rc;
    }
}

    int ObjectBlueprint::decodeBool( const char* data, 
                                     int* idx, 
                                     boost::any* output )
    {
        bool temp;

        if( !decodeBool(data, idx, &temp) ){
            *output = temp;
        } else {
            return -EINVAL;
        }

        return 0;
    }
  
    int ObjectBlueprint::decodeInt( const char* data,
                                    int* idx,
                                    boost::any* output )
    {
        return decodeLong(data, idx, output);
    }

    int ObjectBlueprint::decodeLong( const char* data, 
                                     int* idx, 
                                     boost::any* output )
    {
        long temp;

        if( decodeLong(data, idx, &temp) ){
           *output = temp; 
        } else {
            return -EINVAL;   
        }

        return 0;
    }

    int ObjectBlueprint::decodeString( const char* data, 
                                       int* idx, 
                                       boost::any* output )
    {
        std::string temp;

        if( !decodeString(data, idx, &temp) ){
            *output = temp;
        } else {
            return -EINVAL;
        }
    
        return 0;
    }
    
    int ObjectBlueprint::decodeFloat( const char* data, 
                                      int* idx, 
                                      boost::any* output )
    {
        float temp = 0;
        
        if( !decodeFloat(data, idx, &temp) ){
            *output = temp;
        } else {
            return -EINVAL;
        }
    
        return 0;
    }
    
    int ObjectBlueprint::decodeType( const char* data, 
                                     int* idx, 
                                     boost::any* output )
    {
        Ymir::ObjectType type;

        if( !decodeType(data, idx, &type) ){
            *output = type;
        } else {
            return -EINVAL;
        }
    
        return 0;
    }

    int ObjectBlueprint::decodePropList( const char* data, 
                                         int* idx, 
                                         PropList* output )
    {
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
 

            //Lookup the prop in the blueprint's map
            Blueprint::iterator it;

            //If the entry and its decode function are defined 
            //then call the decodeFP. Otherwise, error.
            if( ((it = mBlueprint.find(prop)) != mBlueprint.end()) && 
                it->second.first )
            {
                it->second.first(data, idx, &val);
            } else {
                return -EINVAL;
            }
        }
    
        //Decode end of list
        if( count && ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }

        return 0;
    }
