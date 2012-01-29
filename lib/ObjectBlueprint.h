#ifndef _OBJECTBLUEPRINT_H
#define _OBJECTBLUEPRINT_H

#include <map>

#include "DecodeBasic.h"
#include "PropList.h"

namespace Ymir {

    class ObjectBlueprint {

        public:

            /*void setProperty(const std::string& name, boost::any& val); 
            bool hasProperty(const std::string& name, boost::any* val);*/
            virtual void create( std::string& id, PropList& props ) = 0;
            virtual void update( std::string& id, PropList& props ) = 0;
            virtual void destroy( std::string& id, PropList& props) = 0;

            //Generic decoder functions
            static int decodeString( const char*, int*, boost::any* );
            static int decodeInt( const char*, int*, boost::any* );
            static int decodeFloat( const char*, int*, boost::any* );
            static int decodeBool( const char*, int*, boost::any* );
            static int decodeLong( const char*, int*, boost::any* );

            //Decoder helper function 
            int decodePropList( const char*, 
                                int* , 
                                Ymir::PropList* );
        
        protected:

            ObjectBlueprint() : mBlueprint() {}
            ~ObjectBlueprint(){};

            /*typedef int (*propDecodeFP)( const std::string&, 
                                         const char*, int*, 
                                         boost::any* );

            virtual int decodeProperty( const char*, int*, boost::any* ) = 0; 

            typedef int (*propDecodeFP)( const std::string&, const char*, int*, boost::any*);
            static int decodePropListBase( propDecodeFP fp[], const char* args, int* idx, Ymir::PropList* output );*/
            typedef int (*decodeFP) (const char*, int*, boost::any*);
            typedef void (*setFP)(void*, boost::any&);

            typedef std::pair<decodeFP, setFP> BPFP;
            typedef std::pair<std::string, BPFP> BPEntry;
            typedef std::map<std::string, BPFP> Blueprint;

           
            void set( void* ptr, PropList& props );

            Blueprint mBlueprint; 
    };
}
#endif
