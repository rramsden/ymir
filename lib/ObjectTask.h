#ifndef OBJECTTASK_H
#define OBJECTTASK_H

#include "Task.h"

namespace Ymir {

    class ObjectTask : Ymir::Task {

        typedef enum {
               INVALID = 0,
               CREATE,
               UPDATE,
               DESTROY,
               MAX 
        } Type;

        public:
            ObjectTask(Ymir::ObjectTask::Type act) : action(act) {}
            ~ObjectTask(){}

            void run(){

                    switch(action){

                        case CREATE:
                            this->create();
                            break;

                        case UPDATE:
                            this->update();
                            break;

                        case DESTROY:
                            printf("TASK DESTROY!!!\n");
                            this->destroy();
                            break;

                       default:
                            //<<HERE>> throw exception
                            break;
                    }
            }

        protected:
            virtual void create( ) = 0;
            virtual void update( ) = 0;
            virtual void destroy( ) = 0;

            Ymir::ObjectTask::Type action;
    };
}
#endif
