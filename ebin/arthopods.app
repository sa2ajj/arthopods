{application, arthopods, [
    {description, "Arthopods Simulated Evolution"},
    {vsn, "0.0"},
    {modules, [
        arthopods,
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
    {mod, {arthopods, []}},
    {env, [
    ]}
]}.
