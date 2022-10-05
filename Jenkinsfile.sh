set -e
set -x
echo "----------------------------------------------"
echo "$JOB_NAME build $BUILD_NUMBER"
WORKSPACE_ROOT="$WORKSPACE/$BUILD_NUMBER"
echo "Setting up build in $WORKSPACE_ROOT"
TEST_OUTPUT_DIR_REL=testcase_output
TEST_OUTPUT_DIR="$WORKSPACE_ROOT/$TEST_OUTPUT_DIR_REL"
TEST_DATA_DIR="$WORKSPACE/../../../test-data"
PROJECTS_DIR_REL="projects"
PROJECTS_DIR=$WORKSPACE_ROOT/$PROJECTS_DIR_REL
mkdir $TEST_OUTPUT_DIR
echo "----------------------------------------------"
echo "\nEnvironment:"
df -h .
echo "----------------------------------------------"
cat /proc/meminfo
echo "----------------------------------------------"
WORKSPACE_ROOT="$WORKSPACE/$BUILD_NUMBER"
echo "Setting up build in $WORKSPACE_ROOT"
TEST_OUTPUT_DIR_REL=testcase_output
TEST_OUTPUT_DIR="$WORKSPACE_ROOT/$TEST_OUTPUT_DIR_REL"
TEST_DATA_DIR="$WORKSPACE/../../../test-data"
PROJECTS_DIR_REL="projects"
PROJECTS_DIR=$WORKSPACE_ROOT/$PROJECTS_DIR_REL

#build using docker file in directory:
IMAGENAME="albus"

cd $PROJECTS_DIR/ALBUS_ionosphere
docker build -t "$IMAGENAME:$BUILD_NUMBER" --no-cache=true .
docker run \
    -v $TEST_DATA_DIR/gfzrnx_2.0-8219_lx64:/optsoft/bin/gfzrnx \
    -v $PROJECTS_DIR/ALBUS_ionosphere/acceptance_tests:/albus_waterhole \
    --workdir /albus_waterhole \
    -v $TEST_OUTPUT_DIR:/test_output \
    --env ALBUS_TESTCASE_OUTPUT=/test_output \
    --rm \
    --user $(id -u jenkins):$(id -g jenkins) \
    "$IMAGENAME:$BUILD_NUMBER" -m nose /albus_waterhole
