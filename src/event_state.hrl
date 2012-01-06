-record( eventState, { keyboard=dict:new(), 
                       mouse= dict:from_list( [{current, {0,0,0}},
                                               {previous, {0,0,0}}] ), 
                       gui = dict:new(),
                       actions = dict:new()} ).
