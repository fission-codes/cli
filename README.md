# Fission CLI

[![Build Status](https://travis-ci.org/fission-suite/web-api.svg?branch=master)](https://travis-ci.org/fission-suite/web-api)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://github.com/fission-suite/blob/master/LICENSE)
[![Built by FISSION](https://img.shields.io/badge/⌘-Built_by_FISSION-purple.svg)](https://fission.codes)
[![Discord](https://img.shields.io/discord/478735028319158273.svg)](https://discord.gg/zAQBDEq)
[![Discourse](https://img.shields.io/discourse/https/talk.fission.codes/topics)](https://talk.fission.codes)

A library and CLI application to help Web 2.0-style applications leverage IPFS in a familiar, compatible way

# QuickStart

## Install dependencies
```shell
# IPFS on MacOS, otherwise https://docs.ipfs.io/introduction/install/
brew install ipfs
brew services start ipfs

# If using Linux, install liblzma-dev
# sudo apt install liblzma-dev
```

# Development

There is a `Makefile` filled with helpful commands. The most used in development is `make watch`.

```shell
# Install development tools
make setup

# Watch project for changes, validating types and syntax
make watch
```
