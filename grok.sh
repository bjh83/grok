#! /bin/sh

# Borrowed from gradle's launcher.

# Attempt to set APP_HOME
# Resolve links: $0 may be a link
PRG="$0"
# Need this for relative symlinks.
while [ -h "$PRG" ] ; do
  ls=`ls -ld "$PRG"`
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    PRG="$link"
  else
    PRG=`dirname "$PRG"`"/$link"
  fi
done
SAVED="`pwd`"
cd "`dirname \"$PRG\"`" >&-
APP_HOME="`pwd -P`"
cd "$SAVED" >&-

java -jar $APP_HOME/target/scala-2.11/grok-assembly-0.1.jar "$@"
