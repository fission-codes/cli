# Fission Live

[![Build Status](https://travis-ci.org/fission-suite/web-api.svg?branch=master)](https://travis-ci.org/fission-suite/web-api)
![License](https://img.shields.io/github/license/fission-suite/cli)
[![Built by FISSION](https://img.shields.io/badge/⌘-Built_by_FISSION-purple.svg)](https://fission.codes)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://fission.codes/discord)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

Seamlessly deploy websites, files, and directories to the decentralized web. 

## QuickStart

On Mac, we're using Homebrew:

```
brew tap fission-suite/fission
brew install fission-cli
```

### Installation 
Grab the latest binary for your operating system from our [release page](https://github.com/fission-suite/cli/releases).

You'll find the most up to date instructions for [installation](https://guide.fission.codes/installation) and [getting started](https://guide.fission.codes/getting-started) in our [Guide](https://guide.fission.codes/).

### Seamless Deployments
Deployments are just one step: `fission up`


```
$ fission up hello-universe/
🚀 Now live on the network
👌 QmRVvvMeMEPi1zerpXYH9df3ATdzuB63R1wf3Mz5NS5HQN
📝 DNS updated! Check out your site at:
🔗 hello-universe.fission.name
```

Simple as that!

If you'd like to redeploy everytime you change a file, use `fission watch`

## Development

There is a `Makefile` filled with helpful commands. The most used in development is `make watch`.

```shell
# Install development tools
make setup

# Watch project for changes, validating types and syntax
make dev
```
