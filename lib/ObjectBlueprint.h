#ifndef _OBJECTBLUEPRINT_H
#define _OBJECTBLUEPRINT_H

#include <map>
#include <list>
#include <vector>
#include <string>

//Decode support
#include <ei.h>
#include <erl_interface.h>

//Used in data generic property maps
#include <boost/any.hpp>

//#include "Task.h"

namespace Ymir {

    typedef enum {
        Invalid = 0,
        Scene,
        Terrain,
        Camera,
        Light, 
        Entity,
        Window,
        Button,
        Max
    } ObjectType;

    class PropList : public std::map<std::string, boost::any> {

        public:
            void setProperty( const std:: string& id, boost::any& val ){
                PropList::iterator it = this->begin();
    
                //If the key already exists, replace it
                if( (it = this->find(id)) != this->end() ){
                    this->erase(id); 
                } 
    
                this->insert(std::pair<std::string, boost::any>(id, val));
            }

            bool hasProperty(const std::string& id, boost::any* val = NULL){
                bool hasProp = false;
                PropList::iterator it = this->begin();
                
                if( (it = this->find(id)) != this->end() ){
                    hasProp = true;
                    
                    if( val ){
                        *val = it->second;
                    }
                }
    
                return hasProp;
            }

            template<typename T>
            bool hasProperty(const std::string& id, T* output){
                bool has = false;
                boost::any temp;

                if( (has = hasProperty(id, &temp)) ){
                    *output = boost::any_cast<T>(temp);
                }

                return has;
            }
    };
    
    //typedef std::map<std::string,boost::any> PropList;

    class ObjectBlueprint {

        public:

            /*void setProperty(const std::string& name, boost::any& val); 
            bool hasProperty(const std::string& name, boost::any* val);*/

            virtual void create( std::string& id, Ymir::PropList& props ) = 0;
            virtual void update( std::string& id, Ymir::PropList& actions ) = 0;
            virtual void destroy( std::string& id ) = 0;

            //Helper functions
            static int decodeString( const char* data, int* idx, std::string* output );
            static int decodeFloat( const char* data, int* idx, float* output );
            static int decodeLong(const char* data, int* idx, long* output);
            static int decodeBool( const char* data, int* idx, bool* output );
            static int decodeType( const char* data, int* idx, Ymir::ObjectType* output );

            //Generic decoder functions
            static int decodeString( const char*, int*, boost::any* );
            static int decodeInt( const char*, int*, boost::any* );
            static int decodeFloat( const char*, int*, boost::any* );
            static int decodeBool( const char*, int*, boost::any* );
            static int decodeLong( const char*, int*, boost::any* );
            static int decodeType( const char*, int*, boost::any* );

            template<typename T>
            static int decodeList( const char* data,
                                   int* idx, 
                                   int (*fp)( const char*, int*, T* ),
                                   std::list<T>* output )
            {
                int count = 0;

                if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
                    return -EINVAL;
                } 

                for( int i = 0; i < count; i++ ){
                    T temp;

                    if( fp(data, idx, &temp) ){
                        return -EINVAL;
                    }

                    output->push_back(temp);
                }

                //Decode end of list
                if( count && ei_decode_list_header(data, idx, &count) ){
                    return -EINVAL;
                }

                return 0;
            }

            template<typename T>
            static int decodeVector( const char* data,
                                     int* idx, 
                                     int (*fp)( const char*, int*, T* ),
                                      std::vector<T>* output )
            {
                int count = 0;

                if( !data || !idx || ei_decode_list_header(data, idx, &count) ){
                    return -EINVAL;
                } 

                for( int i = 0; i < count; i++ ){
                    T temp;

                    if( fp(data, idx, &temp) ){
                        return -EINVAL;
                    }

                    output->push_back(temp);
                }

                //Decode end of list
                if( count && ei_decode_list_header(data, idx, &count) ){
                    return -EINVAL;
                }

                return 0;
            }

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
