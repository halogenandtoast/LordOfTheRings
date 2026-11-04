module LordOfTheRings.Objective.Cards.ToIsengard (toIsengard) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getCaptives, getPursuit, increasePursuit)

newtype ToIsengard = ToIsengard ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Captive heroes are in play but are not under any player's control: each is guarded by
an encounter card (see 'GuardCharacter'), this one or Grishnákh.
-}
toIsengard :: ObjectiveCard ToIsengard
toIsengard = objective ToIsengard Cards.toIsengard

-- | Treat each Captive's text box as blank.
instance HasModifiersFor ToIsengard where
  getModifiersFor (ToIsengard a) = do
    captives <- getCaptives
    for_ captives \h -> modified a h [Blank]

-- | Forced: At the end of the refresh phase, increase the pursuit value by 1.
instance HasAbilities ToIsengard where
  getAbilities (ToIsengard a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

{- | Then, if the pursuit value is 30 or greater, the players lose the game: read once it
has gone up.

If there are no Captives in play, the players lose the game: checked as each moment
passes, the Captives guarded before this card enters play.
-}
instance RunMessage ToIsengard where
  runMessage msg o@(ToIsengard attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      increasePursuit 1
      doStep 1 msg
      pure o
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      pursuit <- getPursuit
      when (pursuit >= 30) $ gameOver Lost
      pure o
    CheckWindows _ -> do
      captives <- getCaptives
      when (null captives) $ gameOver Lost
      ToIsengard <$> runMessage msg attrs
    _ -> ToIsengard <$> runMessage msg attrs
