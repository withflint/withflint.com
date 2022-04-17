#!/bin/bash

echo "Building..."
export GIT_VERSION=$(git rev-parse --short $GITHUB_LONG_HASH)
sort -R .build/quotes | head -n1  >> output
echo "\n<https://github.com/withflint/withflint.com/tree/$GIT_BRANCH|withflint.com/$GIT_BRANCH> @ $GIT_VERSION by $WHO\n\nBuilds:" >> output

declare -A PROJECT_MAP

build() {
    NAME=$1
    PROJECT_BUILDER_SCRIPT=$2
    sh $PROJECT_BUILDER_SCRIPT &
    PROJECT_MAP[$!]=$NAME
    echo "[BUILDING] ${NAME} on pid $!"
}


# add projects here.
build "withflint.com" /.build/withflint.com.sh


for PID in `jobs -p`; do
    if wait $PID; then
        NAME="${PROJECT_MAP[$PID]}"
        echo "BUILD SUCCEED: project $NAME on pid $PID"
        echo "- $NAME Ok.">> output;
    else
        NAME="${PROJECT_MAP[$PID]}"
        CODEBUILD_BUILD_SUCCEEDING=$((CODEBUILD_BUILD_SUCCEEDING-1))
        echo "[ERROR] BUILD FAILED: project $NAME on pid $PID"
        echo "- $NAME Failed!" >> output;
    fi
done

curl -X POST -H "Content-Type:application/json" --data "{ \"text\":\"$(cat output)\"}" "$CHAT_WEBHOOK"