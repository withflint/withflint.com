# withflint.com

Flint's official website. The Front-end is in [Elm](https://elm-lang.org/), and the backend is in F#. This repo aims to showcase some of the internal tech practices that we adopt, promote openness, and do things in public as we are a remote company. It's an important part of our culture.

The website is used to communicate to the industry what we offer, a solution for hiring internationally educated registered nurses. On the other end, explain how it works for nurses and our offering. Lastly, the third responsibility of the website is to host our open positions at Flint.

## Get started
To get started with development, you will need [Elm](https://elm-lang.org/), [yarn](https://yarnpkg.com/) and [dotnet](https://dotnet.microsoft.com/en-us/download) installed. All the working recipes are in the Makefile.

1. install Elm
2. install dotnet
3. run `make install` -- installs a few global yarn packages
4. run `make`
5. your environment should be running.

### Common issues
The newer macOS versions use the port 5000 for AirDrop. You can either disable that feature on your os or run a different port in the code. We could improve this by making the port an environment variable. But you need to make sure you don't commit to the new port. To change it, search 5000 in the code.

You should know that the SMTP server is IP range protected. So it is convenient if you don't want to send out emails uselessly but a little more complicated if you work on it.

One other common issue is installing fantomas and running it. Latest versions of fantomas depends on libhostfxr. [It seems to have something to so with the dotnet root](https://stackoverflow.com/questions/56041783/a-fatal-error-occurred-the-required-library-libhostfxr-dylib-could-not-be-found).
```
A fatal error occurred. The required library libhostfxr.dylib could not be found.
```

You sould make sure that dotnet root and dotnet tools are available in your path.

## Submitting code
In the Makefile recipes, we use Fantomas and [elm-format](https://github.com/avh4/elm-format) to keep the code tidy, but in Elm, we use [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) to apply a set of rules. Ultimately it's the programmer's responsibility to uphold the standard, but [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) is there to help. The principle to think about is if someone else should run [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) or any Makefile command after you, would that cause the code to change. It's something we want to avoid. So run `make fix` before committing to git.

When submitting an increment, you should make a pull request from your form of the repo and use all the best practices promoted in open source. Being a remote company, well, that is how we can be successful.

The repo is open to the public, so you should be mindful of the language you use but also no to expose any secrets. When unsure, ask.

## Building for deployment
The repo is built with GitHub Actions and a docker image is pushed to AWS ECR which is then deployed to Kubernetes using FluxCD.

The build success/failure is available on the Slack _automation_ channel and in GitHub. You can be invited as a contributor.

Deployments are configured and managed in another git repo ([GitOps](https://www.gitops.tech/)). But in principle, the code runs the exact same way.