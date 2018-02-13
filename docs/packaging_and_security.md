# How Enot builds packages?
1. You ask Enot Auto Builder to build package.
2. Enot Auto Builder pulls git tag and builds a package
3. You can use this package as a dep

What is important here?  

    Nobody can push walformed package to EnotHub since we don't accept prebuilt packages.
We build git repositories. And they can be changed by other people.

## I don't trust other people
Just fork the repository you would like to use!  
Enot uses namespaces, so after forking repository just set your github name as a 
package namespace:

    comtihon/mongodb-erlang -> <your_github_acount>/mongodb-erlang
Now you can use your fork of the dep which is much more secure.

## I don't trust Enot Auto Builder
Yes, that's a normal practice. Sometimes you will have your own private library and
sometimes your security requirements will prevent you from using third party 
package storages.  
The answer is simple - launch your own [EnotAutoBuilder](https://github.com/comtihon/enot_auto_builder),
publish there your private packages and use add it to enot's global config (you can completely remove
official Enot repo from it).