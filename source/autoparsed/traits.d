module autoparsed.traits;

import autoparsed.log;

template isInstantiable(T){
  mixin CTLog!("Checking if ", T, " is instantiable");
  //enum isInstantiable = __traits(compiles, (){T x = void;});
  import std.traits;
  //types which are not enums (so, classes, basic types) are instantiable
  //enum types are instantiable if they have at least one member
  enum isInstantiable = isType!(T) && (!is(T == enum) || __traits(allMembers, T).length > 0);
  mixin CTLog!("result: ", isInstantiable);
}


unittest {
  static assert(isInstantiable!int);

  struct S{}
  struct T{int x;}
  enum E;
  enum F { f }

  static assert(isInstantiable!S);
  static assert(isInstantiable!T);
  static assert(!isInstantiable!E);
  static assert(isInstantiable!F);

}
