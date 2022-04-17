#!/bin/sh

export TARGET="$ECR_URL/withflint.com:$GIT_VERSION"

docker build $(pwd) -t $TARGET --build-arg GIT_VERSION=$GIT_VERSION -f $(pwd)/.build/Dockerfile >&1
docker push $TARGET >&1

echo "- published withflint.com:$GIT_VERSION" >> output