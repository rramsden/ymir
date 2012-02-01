#include "DecodeBasic.h"

using namespace std;

namespace Ymir{

    int decodeBool( const char* data, int* idx, bool* output ){
        int temp = 0;
    
        if( !ei_decode_boolean(data, idx, &temp) ){
            *output = (bool)temp;
        } else {
            return -EINVAL;
        }
    
        return 0;
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

    int decodeLong(const char* data, int* idx, long* output){
        return ei_decode_long(data, idx, output);
    }   

    int decodeULong(const char* data, int* idx, unsigned long* output){
        return ei_decode_ulong(data, idx, output);
    }
}
