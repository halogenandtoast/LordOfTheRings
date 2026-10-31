module LordOfTheRings.Treachery.Cards.HazardousTerrain (hazardousTerrain) where

import LordOfTheRings.Trait (Trait (Hazard))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype HazardousTerrain = HazardousTerrain TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hazardousTerrain :: TreacheryCard HazardousTerrain
hazardousTerrain = treachery HazardousTerrain Cards.hazardousTerrain

-- | Surge. Doomed 2.
-- When Revealed: Shuffle all Hazard cards in the encounter discard pile back into the
-- encounter deck.
-- Shadow: Shuffle all Hazard cards in the encounter discard pile back into the encounter
-- deck. Then, deal another shadow card for this attack.
instance RunMessage HazardousTerrain where
  runMessage msg t@(HazardousTerrain attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck (CardWithTrait Hazard)
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      shuffleEncounterDiscardIntoDeck (CardWithTrait Hazard)
      dealShadowCardsDuringAttack eid pid defenders 1
      pure t
    _ -> HazardousTerrain <$> runMessage msg attrs
