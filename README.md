# walrus
Erlang advanced project manager 


### Walrus config

    {
        "name" : AppName,
        "drop_unknown_deps" : Boolean,
        "with_source" : Boolean,
        "deps" : [
            {
                "name" : DepName,
                "url" : DepUrl,
                "vsn" : DepVsn
            }
        ],
        "prebuild" : [
            {Action : Params}
        ],
        "build_vars" : [
            {Var1 : Value}
            Var2
        ]
    }