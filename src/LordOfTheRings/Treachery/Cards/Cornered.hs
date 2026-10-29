module LordOfTheRings.Treachery.Cards.Cornered (cornered) where

import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (attackingEnemyGetsBonus, canRemoveQuestTimeCounter)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Cornered = Cornered TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

cornered :: TreacheryCard Cornered
cornered = treachery Cornered Cards.cornered

-- | When Revealed: The first player either removes 1 time counter from the current quest or
-- each enemy gets -20 engagement cost and +1 attack until the end of the round.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player's threat
-- is 20 or less).
instance RunMessage Cornered where
  runMessage msg t@(Cornered attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      removable <- canRemoveQuestTimeCounter
      enemies <- select AnyEnemy
      chooseOrRunOneM lead do
        questionLabeled "cornered.choose"
        causedBy attrs
        when removable $ labeled "cornered.removeTimeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
        labeled "cornered.enemies" $ for_ enemies \e ->
          roundModifiers attrs e [EngagementCostModifier (-20), AttackModifier 1]
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attackingEnemyGetsBonus attrs eid pid
      pure t
    _ -> Cornered <$> runMessage msg attrs
