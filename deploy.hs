#!/usr/bin/env stack
{- stack
  runghc
  --stack-yaml=deploy/stack.yaml
  --package deploy-twinpin
-}
module Deploy where

import DeployTwinpin (deployTwinpin)

main :: IO ()
main = deployTwinpin