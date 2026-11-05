module LordOfTheRings.Game.Dispatch (module LordOfTheRings.Game.Dispatch) where

import LordOfTheRings.Ally ()
import LordOfTheRings.Attachment ()
import LordOfTheRings.Classes.HasQueue (runQueueT)
import LordOfTheRings.Classes.RunMessage (RunMessage (..))
import LordOfTheRings.Contract ()
import LordOfTheRings.Enemy ()
import LordOfTheRings.Entities (
  Entities,
  alliesL,
  attachmentsL,
  contractsL,
  enemiesL,
  heroesL,
  locationsL,
  objectivesL,
  questsL,
  treacheriesL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Hero ()
import LordOfTheRings.Location ()
import LordOfTheRings.Message (Message)
import LordOfTheRings.Objective ()
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest ()
import LordOfTheRings.Treachery ()

runEach :: RunMessage a => Message -> a -> GameT a
runEach msg = runQueueT . runMessage msg

runEntities :: Message -> Entities -> GameT Entities
runEntities msg entities =
  traverseOf (heroesL . traverse) (runEach msg) entities
    >>= traverseOf (alliesL . traverse) (runEach msg)
    >>= traverseOf (locationsL . traverse) (runEach msg)
    >>= traverseOf (enemiesL . traverse) (runEach msg)
    >>= traverseOf (treacheriesL . traverse) (runEach msg)
    >>= traverseOf (questsL . traverse) (runEach msg)
    >>= traverseOf (attachmentsL . traverse) (runEach msg)
    >>= traverseOf (objectivesL . traverse) (runEach msg)
    >>= traverseOf (contractsL . traverse) (runEach msg)

