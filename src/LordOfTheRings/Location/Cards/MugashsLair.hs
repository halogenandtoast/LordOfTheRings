module LordOfTheRings.Location.Cards.MugashsLair (mugashsLair) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (revealTopOfOutOfPlayDeck)

newtype MugashsLair = MugashsLair LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mugashsLair :: LocationCard MugashsLair
mugashsLair = location MugashsLair Cards.mugashsLair

-- | Forced: At the beginning of the encounter phase, reveal the top card of each player's
-- out-of-play deck. Add each revealed enemy to the staging area and discard the rest.
instance HasAbilities MugashsLair where
  getAbilities (MugashsLair a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

-- | Shadow: Defending player discards an attachment he controls.
instance RunMessage MugashsLair where
  runMessage msg l@(MugashsLair a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer revealTopOfOutOfPlayDeck
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      chooseAndDiscard (EnemySource eid) pid attachments
      pure l
    _ -> MugashsLair <$> runMessage msg a
