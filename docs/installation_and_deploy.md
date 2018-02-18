# How to enable Enot easy deploy for your Erlang service/tool?
## What is easy deploy?
Imagine you have some useful cli tool (or service) written in Erlang and you want every
person to be able to install and run it everywhere(`*`)?  
For Python they have [pip](https://pypi.python.org/pypi/pip). You run `pip install enot`
and get `enot` ready to use.  
For Erlang you have [enot](https://enot.justtech.blog/) now. You run 
`enot install your_github_name/your_project` and get `your_project` ready to use.

## Why do I need easy deploy?
If you want your software to be used - you should make it's installation process easy and fast.  
Compare steps before Enot:
* find and install proper version of Erlang
* if you don't have Erlang package in your distro - download all requirements and build it manually
* download and build package (in case of any error all users not familiar with Erlang end here)
* make it runnable (script/systemd/other)
* run 

With steps after Enot:
* `pip install enot`
* `enot install you/your_tool`


## How can I add easy deploy to my project?
First you should add installation steps to your `enot_config.json`. Read [here](project_configuration.md) about configuration
format.

        "install" : [
         {"release": {"rel_dir" : "/opt"}},
         {"shell": "ln -s /opt/_rel/your_tool/bin/your_tool /usr/bin/your_tool"}
        ]
Second you should add your service to [EnotHub](https://github.com/comtihon/enot_auto_builder/blob/master/docs/Add.md).  
That's is all. 
Just replace your installation instructions with `enot install your_github_name/your_tool` in the Readme.  
`your_tool` will be available in the system after installation succeeds.

## Do I need Erlang on the target machine?
No (`**`).  
Every installation is shipped with prebuilt `erts` for your system(`*`). When user run `enot install ...` 
 it is downloaded from EnotHub.

## And what about deps, will they be built during installation?
All your prebuilt deps are also downloaded from EnotHub. 
If some of your deps don't exist there - they will be downloaded and built locally. But you 
will need all Erlang toolchain installed to built them.

## Typos

(`*`) This is not true. For now this feature works only for Linux systems with musl C library 
implementation. Later it will work for both musl and glibc.  
(`**`) This works only when all deps were loaded to EnotHub. Make sure to read _"How to add deps of my package?"_
[doc](https://github.com/comtihon/enot_auto_builder/blob/master/docs/Add.md) before manually adding each dep :)