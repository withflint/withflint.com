FROM mcr.microsoft.com/dotnet/sdk:6.0

ARG GIT_VERSION=${GIT_VERSION}
ENV GIT_VERSION=${GIT_VERSION}

workdir /build

# ELM
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin/
RUN elm --help

#COPY
COPY static static
COPY index.html index.html
COPY backend backend
COPY elm elm

# BUILD ELM
RUN mkdir -p static/${GIT_VERSION}
RUN cd elm && elm make --optimize --output ../static/${GIT_VERSION}/elm.js src/Main.elm
RUN cd ..
RUN sed -i "s/GIT_VERSION/${GIT_VERSION}/g" index.html

# BUILD DOTNET
RUN dotnet publish backend/ -c Release -o bin

FROM mcr.microsoft.com/dotnet/aspnet:6.0

WORKDIR /

ARG GIT_VERSION=${GIT_VERSION}
ENV GIT_VERSION=${GIT_VERSION}

COPY --from=0 /build/static/ /static/
COPY --from=0 /build/index.html index.html
COPY --from=0 /build/bin/* /bin/
COPY content content

EXPOSE 5000

ENTRYPOINT ["dotnet", "bin/backend.dll"]