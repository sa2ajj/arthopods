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
        {default_energy, 200},
        {turn_cost, 1},
        {turn_time, 20},
        {move_cost, 2},
        {move_time, 100},
        {food_energy, 10},
        {split_energy, 700},
        {world_size, {700, 400}},
        {food_frequency, 20},
        {initial_coverage, 0.05},
        {bugs_to_create, 1},
        {cell_size, 2},
        {bug_colour, {16#b0, 16#30, 16#60}},
        {grass_colour, {0, 16#64, 0}}
    ]}
]}.
