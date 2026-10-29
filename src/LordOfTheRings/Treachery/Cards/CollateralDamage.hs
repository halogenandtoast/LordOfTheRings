module LordOfTheRings.Treachery.Cards.CollateralDamage (collateralDamage) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (damageToScrollHero)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CollateralDamage = CollateralDamage TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

collateralDamage :: TreacheryCard CollateralDamage
collateralDamage = treachery CollateralDamage Cards.collateralDamage

-- | When Revealed: Discard 2 cards from the top of the encounter deck. Discard an
-- additional 2 cards for each copy of Collateral Damage in the discard pile. Then, raise
-- each player's threat by 2 for each location discarded by this effect.
-- Shadow: Any damage dealt by this attack is dealt to the hero with Alcaron's Scroll
-- attached, if able.
instance RunMessage CollateralDamage where
  runMessage msg t@(CollateralDamage attrs) = case msg of
    -- sentence by sentence: a copy the first 2 discard counts for the second (ruling); each
    -- batch's locations raise the threat as it is discarded
    WhenRevealed (isTarget attrs -> True) -> do
      discardTopOfEncounterDeck attrs 2
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let copies = length [c | c <- pile, toCardCode c == toCardCode Cards.collateralDamage]
      when (copies > 0) $ discardTopOfEncounterDeck attrs (2 * copies)
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      let locations = length [c | c <- cards, cdCardType (toCardDef c) == LocationType]
      when (locations > 0) $ eachPlayer \p -> encounterRaisesThreat p (2 * locations)
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damageToScrollHero eid defenders
      pure t
    _ -> CollateralDamage <$> runMessage msg attrs
