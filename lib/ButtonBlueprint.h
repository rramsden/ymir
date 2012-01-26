#ifndef _BUTTONBLUEPRINT_H
#define _BUTTONBLUEPRINT_H

#include "MyGUIBlueprint.h"

namespace Ymir {

    class ButtonBlueprint : public MyGUIBlueprint<MyGUI::Button> {

        public:
            ButtonBlueprint();
            ~ButtonBlueprint(){};
    };
}
#endif
