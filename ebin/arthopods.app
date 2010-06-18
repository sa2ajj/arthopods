{application, arthopods, [
    {description, ""},
    {vsn, "0.0"},
    {modules, [
        arthopods_app,
        arthopods_sup,
        arthopod,
        arthopod_simple,
        arthopod_sup,
        food_generator,
        grass_field,
        main,
        select,
        utils,
        world,
        world_viewer
    ]},
    {registered, [
    ]},
    {applications, [
        kernel,
        stdlib
    ]},
    {mod, {arthopods_app, []}},
    {env, [
    ]}
]}.
