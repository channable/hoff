-- Hoff -- A gatekeeper for your commits
-- Copyright 2016 Ruud van Asseldonk
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- A copy of the License has been included in the root of the repository.

import Test.Hspec (hspec)
import EventLoopSpec (eventLoopSpec)
import ServerSpec (serverSpec)
import AlembicRebaseSpec (alembicSpec)

-- This test suite tests interaction of the system with the outside world, as
-- opposed to its internals (there are unit tests for that). It is not a full
-- end-to-end test, in the sense that the final executable is tested. Instead,
-- the program is tested in two stages: one tests the Git interaction and the
-- event loop, but incoming messages are faked. The other tests the web server,
-- ignoring the messages it receives.

main :: IO ()
main = hspec $ do
  eventLoopSpec
  serverSpec
  alembicSpec
