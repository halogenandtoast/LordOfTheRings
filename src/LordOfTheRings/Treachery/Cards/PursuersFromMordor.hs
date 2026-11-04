module LordOfTheRings.Treachery.Cards.PursuersFromMordor (pursuersFromMordor) where

import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (addOrcToOrcsArea, orcEnemy, placeOrcsProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PursuersFromMordor = PursuersFromMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2.
pursuersFromMordor :: TreacheryCard PursuersFromMordor
pursuersFromMordor = treachery PursuersFromMordor Cards.pursuersFromMordor

-- | When Revealed: Discard cards from the top of the encounter deck until an Orc enemy is
-- discarded. Add that enemy to the Orcs' area.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place 2
-- progress on the Orcs' stage.
instance RunMessage PursuersFromMordor where
  runMessage msg t@(PursuersFromMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardEncounterCardsUntil attrs orcEnemy
      pure t
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      addOrcToOrcsArea cards
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeOrcsProgress 2
      pure t
    _ -> PursuersFromMordor <$> runMessage msg attrs
