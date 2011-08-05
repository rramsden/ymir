#ifndef YMIROBJECT_H
#define YMIROBJECT_H

#include <map>
#include <string>

//Decode support
#include <ei.h>
#include <erl_interface.h>

//Used in data generic property maps
#include <boost/any.hpp>

namespace Ymir {

    typedef enum {
        OBJECT_INVALID = 0,
        OBJECT_CAMERA,
        OBJECT_LIGHT, 
        OBJECT_ENTITY,
        OBJECT_WINDOW,
        OBJECT_BUTTON,
        OBJECT_MAX
    } ObjectType;

    int decodeString( const char* data, int* idx, std::string* output );
    int decodeBool( const char* data, int* idx, int* output );
    int decodeType( const char* data, int* idx, Ymir::ObjectType* output );

    typedef std::map<std::string,boost::any> PropList;

    class YmirObject {
    
        public:
            YmirObject( const std::string& uuid );
            ~YmirObject( );
    
            void setProperty(const std::string& name, boost::any& val);
            bool hasProperty(const std::string& name, boost::any* val);
    
            /* Pure virtuals, determined by type of Ymir object */
            virtual int decodeProps( const char* args, int* idx ) = 0;
            virtual int decodeProp( const std::string& prop, const char* args, int* idx ) = 0;
    
        protected:
    
            std::string uuid;
            PropList props;
    };
}
#endif
