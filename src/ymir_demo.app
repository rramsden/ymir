{application, ymir_demo, 
    [
        {description, "Ymir - Proof of Concept"},
        {vsn, 0.1},
        {modules, [ymir_demo_app, ymir_demo_sup, ymir_demo]},
    
        {registered, []},
    
        {applications, [kernel, stdlib]},
    
        {included_applications, []},
    
        {env, []},
    
        {mod, {ymir_demo_app, []}}
    ]
}.
