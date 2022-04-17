#!/bin/sh

export TARGET="$ECR_URL/withflint.com:$GIT_VERSION"

docker build $(pwd) -t $TARGET --build-arg GIT_VERSION=$GIT_VERSION -f $(pwd)/.build/Dockerfile
docker push $TARGET