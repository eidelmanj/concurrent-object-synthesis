# cd ~/sketch-newest/sketch-backend
# make
# cd ~/sketch-newest/sketch-frontend
# make run-local-seq EXEC_ARGS="--fe-output-code /home/eidelmanj/Dropbox/school/Toronto/Research/synthesis/space_exploration/c++/parser/$1"
cd test
export SKETCH_HOME=/home/eidelmanj/sketch-newest/sketch-frontend

    export MAVEN_OPTS="-XX:MaxPermSize=256m -Xms40m -Xmx600m -ea -server"
    mvn -o -f "${SKETCH_HOME}"/pom.xml -e compile exec:java \
      -Dexec.mainClass=sketch.compiler.main.seq.SequentialSketchMain \
      -Dexec.args="$* --fe-output-code --fe-output-test"

 $1



