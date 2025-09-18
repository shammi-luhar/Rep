#!/bin/bash

# Exit immediately if any command fails
set -e

# Print each command before executing
set -x

# Install pre-commit hooks into .git/hooks
pre-commit install

# Run all pre-commit hooks on all tracked files
pre-commit run --all-files
