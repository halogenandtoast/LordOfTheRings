module LordOfTheRings.Window (module LordOfTheRings.Window, module X) where

import LordOfTheRings.Id
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait
import LordOfTheRings.Timing as X

data Window = Window {windowTiming :: Timing, windowType :: WindowType}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowType
  = EntersPlay Target
  | PhaseBegins Phase
  | CommittedToQuest Target
  | EnemyEngaged EnemyId PlayerId
  | DealtDamage Target Int
  | Traveled LocationId
  | DeclaredAsDefender Target EnemyId
  | -- | a character left play: who controlled it, and its traits as it left
    LeftPlay Target PlayerId (Set Trait)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkWhen :: WindowType -> Window
mkWhen = Window When

mkAfter :: WindowType -> Window
mkAfter = Window After
