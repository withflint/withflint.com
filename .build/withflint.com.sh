#!/bin/sh

docker build $(pwd) -t $(ECR_URL)/withflint.com:$(GIT_VERSION) --build-arg GIT_VERSION=$(GIT_VERSION) -f $(pwd)/.build/Dockerfile  >&1
docker push $(ECR_URL)/withflint.com:$(GIT_VERSION)
echo "- published withflint.com:$(GIT_VERSION)" >> output