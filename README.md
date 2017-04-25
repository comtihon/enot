# Coon
Erlang advanced project manager 


### coon config

    {
        "name" : AppName,
        "version" : AppVsn,
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
        ],
        "c_build_vars" : [
            {VarName1 : VarValue1}
        ]
    }