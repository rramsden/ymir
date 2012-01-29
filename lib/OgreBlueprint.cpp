#include "OgreBlueprint.h"

using namespace Ogre;

namespace Ymir {
int OgreBlueprint::decodeReal( const char* data, 
                               int* idx, 
                               Ogre::Real* output )
{
    double val = 0;

    if( !ei_decode_double(data, idx, &val) ){
        *output = Ogre::Real(val);
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeRadian( const char* data, 
                                 int* idx, 
                                 Ogre::Radian* output )
{
    double val = 0;

    if( !ei_decode_double(data, idx, &val) ){
        *output = Ogre::Radian(val);
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeColourVal( const char* data, 
                                    int* idx, 
                                    Ogre::ColourValue* output )
{
    int arity = 0;
    double r = 0, g = 0, b = 0, a = 0;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 4) ){
        return -EINVAL;
    } 

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) ) {
            return -EINVAL;
        }

        switch(i){

            case 0:
                r = val;
                break;

            case 1:
                g = val;
                break;

            case 2:
                b = val;
                break;
            
            case 3:
                a = val;
                break;
        }

    }

    *output = Ogre::ColourValue(r,g,b,a);

    return 0;
}

int OgreBlueprint::decodeVector3( const char* data, 
                                  int* idx , 
                                  Ogre::Vector3* output )
{
    int arity = 0;
    Real x, y, z;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 3) ){
        return -EINVAL;
    }

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) )
        {
            return -EINVAL;
        }

        switch(i){

            case 0:
                x = val;
                break;

            case 1:
                y = val;
                break;

            case 2:
                z = val;
                break;
        }
    }

    *output = Ogre::Vector3(x,y,z);

    return 0;    
}

int OgreBlueprint::decodeVector4( const char* data, 
                                  int* idx, 
                                  Ogre::Vector4* output )
{
    int arity = 0;
    Real x, y, z, w;

    if( ei_decode_tuple_header(data, idx, &arity) || (arity != 4) ){
        return -EINVAL;
    }

    for( int i = 0; i < arity; i++ ){
        double val = 0;

        if( ei_decode_double(data, idx, &val) ){
            return -EINVAL;
        }

        switch(i){

            case 0:
                x = val;
                break;

            case 1:
                y = val;
                break;

            case 2:
                z = val;
                break;

            case 3: 
                w = val;
                break;
        }
    }

    *output = Ogre::Vector4(x,y,z, w);

    return 0;
}

int OgreBlueprint::decodeReal( const char* data, 
                               int* idx, 
                               boost::any* output )
{
    Ogre::Real temp;

    if( !decodeReal(data, idx, &temp) ){
        *output = temp;
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeRadian( const char* data, 
                                 int* idx, 
                                 boost::any* output )
{
    Ogre::Radian temp;

    if( !decodeRadian(data, idx, &temp) ){
        *output = temp;
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeColourVal( const char* data, int* idx, boost::any* output)
{
    Ogre::ColourValue temp;

    if( !decodeColourVal(data, idx, &temp) ){
        *output = temp;
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeVector3( const char* data, int* idx, boost::any* output )
{
    Ogre::Vector3 temp;

    if( !decodeVector3(data, idx, &temp) ){
        *output = temp;
    } else {
        return -EINVAL;
    }

    return 0;
}

int OgreBlueprint::decodeVector4( const char* data, int* idx, boost::any* output)
{
    Ogre::Vector4 temp;

    if( !decodeVector4(data, idx, &temp) ){
        *output = temp;
    } else {
        return -EINVAL;
    }

    return 0;
}

}
