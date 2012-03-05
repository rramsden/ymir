#ifndef _MYGUIOBJECT_H
#define _MYGUIOBJECT_H

//MyGUI
#include <MyGUI.h>

#include "Object.h"

namespace Ymir {

    template <class T>
    class MyGUIObject : public Object {
       
        public:

            ~MyGUIObject() {
                MyGUI::Gui* gui = MyGUI::Gui::getInstancePtr();

                if( gui && mObject ){
                    gui->destroyWidget(mObject);
                }
            }
  
            friend class MyGUIBlueprint<T>;

        protected:

            MyGUIObject( const std::string& uuid, Object::Type type ) 
                : Ymir::Object(uuid, type) 
            {
                mObject = NULL;
            }

            T* mObject;
    };
}
#endif
