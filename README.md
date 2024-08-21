# Monkey CPP

Monkey CPP is a modern C++ implementation of Monkey from 'writing an interpreter in Go' by Thorsten Ball.

## Current State

Currently, this repo represents a "I wanted to do it one way, but had to pivot to get it done on the weekend, so it's a mess" state.
Build pipeline does not work as we need the latest clang and related standard library, and it's a pain at the moment to get it right on GitHub's build image.

## Goals

- `constexpr`/`consteval`-able
- minimal allocations
