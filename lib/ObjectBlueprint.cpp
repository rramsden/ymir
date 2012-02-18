#include "ObjectBlueprint.h"
#include "DecodeBasic.h"

#include "Core.h"

using namespace std;
using namespace boost;

namespace Ymir {

    void ObjectBlueprint::set( void* ptr, PropList& props ){
        PropList::iterator pIT;
        Blueprint::iterator bIT;

        for( pIT = props.begin(); pIT != props.end(); pIT++ ){
            bIT = mBlueprint.find(pIT->first);

            if( (bIT != mBlueprint.end()) && bIT->second.second ){
                bIT->second.second(ptr, pIT->second);
            }
        }
    }

    int ObjectBlueprint::decodeBool( const char* data, 
                                     int* idx, 
                                     boost::any* output )
    {
        bool temp;

        if( !Ymir::decodeBool(data, idx, &temp) ){
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

        if( Ymir::decodeLong(data, idx, &temp) ){
            return -EINVAL;
        } 
        
        *output = temp;

        return 0;
    }

    int ObjectBlueprint::decodeULong( const char* data,
                                      int* idx,
                                      boost::any* output )
    {
        unsigned long temp;

        if( Ymir::decodeULong(data, idx, &temp) ){
            return -EINVAL;
        } 
        
        *output = temp;

        return 0;
    }

    int ObjectBlueprint::decodeString( const char* data, 
                                       int* idx, 
                                       boost::any* output )
    {
        std::string temp;

        if( !Ymir::decodeString(data, idx, &temp) ){
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
        
        if( !Ymir::decodeFloat(data, idx, &temp) ){
            *output = temp;
        } else {
            return -EINVAL;
        }
    
        return 0;
    }
    
    /*int ObjectBlueprint::decodeType( const char* data, 
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
    }*/

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
                Ymir::decodeString(data, idx, &prop) )
            {
                Core::getSingletonPtr()->logCritical("Failed to decode property name!");
                return -EINVAL;
            }
 

            //Lookup the prop in the blueprint's map
            Blueprint::iterator it;

            if( ((it = mBlueprint.find(prop)) == mBlueprint.end()) ||
                !(it->second.first) )
            {
                Core::getSingletonPtr()->logCritical("No decoding function specified for prop: " + prop);
                return -EINVAL;
            }

            //If the entry and its decode function are defined 
            //then call the decodeFP. Otherwise, error.
            if( it->second.first(data, idx, &val) ){

                Core::getSingletonPtr()->logCritical("Failed to decode property: " + prop);
                return -EINVAL;
            }
            
            //Add the entry to the output
            output->insert(PropListEntry(prop, val));
        }
    
        //Decode end of list
        if( count && ei_decode_list_header(data, idx, &count) ){
            return -EINVAL;
        }

        return 0;
    }
}
