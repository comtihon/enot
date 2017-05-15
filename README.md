# Coon [![Build Status](https://travis-ci.org/comtihon/coon.svg?branch=master)](https://travis-ci.org/comtihon/coon)
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
    
### Testing
    
    python setup.py install
All:

    make tests
Single Testcase:

    py.test -q -s test/test_module.py::TestClass