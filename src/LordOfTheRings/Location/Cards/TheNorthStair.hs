module LordOfTheRings.Location.Cards.TheNorthStair (theNorthStair) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheNorthStair = TheNorthStair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNorthStair :: LocationCard TheNorthStair
theNorthStair = location TheNorthStair Cards.theNorthStair

-- | Forced: After traveling to The North Stair, move the top card of the encounter discard
-- pile to the staging area. Resolve any "when revealed" effects on that card.
instance HasAbilities TheNorthStair where
  getAbilities (TheNorthStair a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage TheNorthStair where
  runMessage msg l@(TheNorthStair a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      top <- getsGame (headMay . (.encounterDiscard) . toAttrs . (.scenario))
      for_ top revealFromEncounterDiscard
      pure l
    _ -> TheNorthStair <$> runMessage msg a
