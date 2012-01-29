#ifndef _DECODEBASIC_H
#define _DECODEBASIC_H

#include <list>
#include <vector>
#include <string>

//Decode support
#include <ei.h>
#include <erl_interface.h>

namespace Ymir {

            //Type specific decoding helpers
    int decodeString( const char* data, int* idx, std::string* output );
    int decodeFloat( const char* data, int* idx, float* output );
    int decodeLong(const char* data, int* idx, long* output);
    int decodeBool( const char* data, int* idx, bool* output );


   template<typename T>
   int decodeList( const char* data,
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
   int decodeVector( const char* data,
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

}
#endif
