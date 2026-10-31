module LordOfTheRings.Treachery.Cards.PressedOnAllSides (pressedOnAllSides) where

import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (controlledLocations)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PressedOnAllSides = PressedOnAllSides TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pressedOnAllSides :: TreacheryCard PressedOnAllSides
pressedOnAllSides = treachery PressedOnAllSides Cards.pressedOnAllSides

-- | When Revealed: If the total threat of locations controlled by players is greater than
-- the total threat in the staging area, raise each player's threat by 2 and immediately
-- end the quest phase without resolving the quest.
-- Shadow: Attacking enemy gets +1 attack for each location you control.
instance RunMessage PressedOnAllSides where
  runMessage msg t@(PressedOnAllSides attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      controlled <- sum <$> (traverse getLocationThreat =<< select (LocationControlledBy AnyPlayer))
      staging <- getStagingThreat
      when (controlled > staging) do
        eachPlayer \pid -> encounterRaisesThreat pid 2
        endQuestPhaseWithoutResolving
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      n <- length <$> controlledLocations pid
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> PressedOnAllSides <$> runMessage msg attrs
