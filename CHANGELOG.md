# Changelog for twinpin

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Changed
* Switched from using bmp images to embedded vector graphics for the textures

### Added
* Limited amount of shots per second for each player

## [0.1.1] - 2019-12-21
### Fixed
* Corrected the refresh rate to work for a 60hz monitor

## [0.1.0] - 2019-12-20
### Added
* A window where the game is drawn
* Two players that respond to gamepad input
  * They move on left thumbstick movement
  * They rotate on right thumbstick movement
  * They fire a shot when the right bumber button is pressed
* Shots turn red when passing through players
* Bounds, which are visualized by the window edges, limit the position of the
  players
* Player to player collision
* Pillars that limits the positions of the players and shots

[Unreleased]: https://github.com/victorjoh/twinpin/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/victorjoh/twinpin/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/victorjoh/twinpin/releases/tag/v0.1.0