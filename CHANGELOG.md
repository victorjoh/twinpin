# Changelog for twinpin

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2020-06-30
### Added
* Added player boost capabilities triggered with left bumper button or left
  trigger button.

### Changed
* The gun reload animation is now used for boost recharge instead.

### Fixed
* Improved performance for drawing players

## [0.3.0] - 2020-02-25
### Added
* Added player health and bullet damage. Player health is represented by a
  circle sector on the player visual.
* Game is now over when a player's lives are depleted. Each player starts with 5
  lives. When the game is over, a menu is show where you can either restart or
  quit.

## [0.2.2] - 2020-02-14
### Fixed
* Fixed bug where framerate was not locked at max 60 Hz

## [0.2.1] - 2020-02-13
### Added
* Gamepads connected after the game started can now be used. The same goes for
  reconnected gamepads.

### Fixed
* When a player is wedged between a pillar and an other player, it can now
  properly slide off the other player when moving.

## [0.2.0] - 2020-02-03
### Changed
* Switched from using bmp images to embedded vector graphics for the textures
* Changed to fullscreen mode and made the aspect ratio a litter wider

### Added
* Limited amount of bullets per second for each player
* Players keep firing while trigger is pressed
* Made the players have different colors
* Players can now also fire using the right trigger
* Added player reload animation
* Added pause menu

## [0.1.1] - 2019-12-21
### Fixed
* Corrected the refresh rate to work for a 60 Hz monitor

## [0.1.0] - 2019-12-20
### Added
* A window where the game is drawn
* Two players that respond to gamepad input
  * They move on left thumbstick movement
  * They rotate on right thumbstick movement
  * They fire a bullet when the right bumber button is pressed
* Bullets turn red when passing through players
* Bounds, which are visualized by the window edges, limit the position of the
  players
* Player to player collision
* Pillars that limits the positions of the players and bullets

[Unreleased]: https://github.com/victorjoh/twinpin/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/victorjoh/twinpin/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/victorjoh/twinpin/compare/v0.2.2...v0.3.0
[0.2.2]: https://github.com/victorjoh/twinpin/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/victorjoh/twinpin/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/victorjoh/twinpin/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/victorjoh/twinpin/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/victorjoh/twinpin/releases/tag/v0.1.0