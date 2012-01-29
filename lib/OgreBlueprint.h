#ifndef _OGREBLUEPRINT_H
#define _OGREBLUEPRINT_H

#include <OgrePrerequisites.h>
#include <OgreMath.h>
#include <OgreColourValue.h>
#include <OgreVector3.h>
#include <OgreVector4.h>

#include "ObjectBlueprint.h"

namespace Ymir {

    class OgreBlueprint : public Ymir::ObjectBlueprint {

        public:

            virtual void create( std::string& id, Ymir::PropList& props ) = 0;
            virtual void update( std::string& id, Ymir::PropList& props ) = 0;
            virtual void destroy( std::string& id, Ymir::PropList& props ) = 0;

            static int decodeReal( const char*, int*, Ogre::Real* );
            static int decodeRadian( const char*, int*, Ogre::Radian* );
            static int decodeColourVal( const char*, int*, Ogre::ColourValue* );
            static int decodeVector3( const char*, int*, Ogre::Vector3* );
            static int decodeVector4( const char*, int*, Ogre::Vector4* );

            static int decodeReal( const char*, int*, boost::any* );
            static int decodeRadian( const char*, int*, boost::any* );
            static int decodeColourVal( const char*, int*, boost::any* );
            static int decodeVector3( const char*, int*, boost::any* );
            static int decodeVector4( const char*, int*, boost::any* );

            protected:
                
                OgreBlueprint() : Ymir::ObjectBlueprint() {} 
                ~OgreBlueprint(){}    
    };

}

#endif
