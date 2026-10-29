module LordOfTheRings.Treachery.Cards.TrappedInside (trappedInside) where

import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardRescuedVillagers, discardVillagers)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TrappedInside = TrappedInside TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trappedInside :: TreacheryCard TrappedInside
trappedInside = treachery TrappedInside Cards.trappedInside

-- | Doomed 2.
-- When Revealed: Discard 1 villager token from the active location for each player in the
-- game. If no villager tokens were removed by this effect, Trapped Inside gains Surge.
-- Shadow: If this attack destroys a character, discard 1 villager token from Rescued
-- Villagers.
instance RunMessage TrappedInside where
  runMessage msg t@(TrappedInside attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      discarded <- sum <$> (traverse (\l -> discardVillagers attrs l players) =<< select IsActiveLocation)
      when (discarded == 0) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      discardRescuedVillagers attrs 1
      pure t
    _ -> TrappedInside <$> runMessage msg attrs
