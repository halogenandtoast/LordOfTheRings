module LordOfTheRings.Treachery.Cards.NoCampfire (noCampfire) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoCampfire = NoCampfire TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

noCampfire :: TreacheryCard NoCampfire
noCampfire = treachery NoCampfire Cards.noCampfire

-- | Doomed 1.
-- When Revealed: Each player must choose one: increase his threat by 4, or reveal an
-- additional encounter card from the encounter deck and add it to the staging area.
-- Shadow: The defending player raises his threat by 2.
instance RunMessage NoCampfire where
  runMessage msg t@(NoCampfire attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "noCampfire.choose"
        causedBy attrs
        labeled "noCampfire.threat" $ encounterRaisesThreat p 4
        labeled "noCampfire.reveal" revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      encounterRaisesThreat pid 2
      pure t
    _ -> NoCampfire <$> runMessage msg attrs
