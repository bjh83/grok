Structure of Type Checker
=========================
Unfortunately, symbol resolution on Grok is very difficult. First off, since
function overloading is allowed, we need to know all the types which may be used
in a file in order to resolve the version of the function. Thus, before we do
anything involving resolution, we must scan the compilation unit once to
determine all top level definitions (Lower scopes do not matter since something
must be declared before referenced if not in top level scope).
