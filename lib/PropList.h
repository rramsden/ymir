#ifndef _PROPLIST_H
#define _PROPLIST_H

#include <map>
#include <string>
#include <boost/any.hpp>

namespace Ymir {
    typedef std::pair<std::string, boost::any> PropListEntry;

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
}
#endif
