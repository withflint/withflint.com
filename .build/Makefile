build-image:
		docker build $(TARGET) -t $(ECR_URL)/withflint.com:$(GIT_VERSION) --build-arg GIT_VERSION=$(GIT_VERSION) -f $(DOCKERFILE)  >&1
		docker push $(ECR_URL)/withflint.com:$(GIT_VERSION)
		echo "- published withflint.com:$(GIT_VERSION)" >> output