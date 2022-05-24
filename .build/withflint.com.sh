#!/bin/sh

export TARGET="$ECR_URL/withflint.com:$GIT_VERSION"

nix build .#withflint-image
skopeo copy docker-archive:result docker://$TARGET
echo "- published withflint.com:$GIT_VERSION"
