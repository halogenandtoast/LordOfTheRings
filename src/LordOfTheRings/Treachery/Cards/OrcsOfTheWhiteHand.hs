module LordOfTheRings.Treachery.Cards.OrcsOfTheWhiteHand (orcsOfTheWhiteHand) where

import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (defendersWeakerThan)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OrcsOfTheWhiteHand = OrcsOfTheWhiteHand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcsOfTheWhiteHand :: TreacheryCard OrcsOfTheWhiteHand
orcsOfTheWhiteHand = treachery OrcsOfTheWhiteHand Cards.orcsOfTheWhiteHand

{- | When Revealed: Remove all damage from each enemy at this stage. Each enemy at this stage
gets +1 threat, +1 attack, and +1 defense until the end of the round. The enemies at this
stage are those in its staging area and those engaged with its players.
Shadow: If the defending character has fewer printed hit points than the attacking enemy,
this attack is considered undefended.
-}
instance RunMessage OrcsOfTheWhiteHand where
  runMessage msg t@(OrcsOfTheWhiteHand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      staged <- select EnemyInStagingArea
      engaged <- select (EnemyEngagedWith PlayerAtThisStage)
      for_ (staged <> engaged) \e -> do
        healAllDamage attrs e
        roundModifiers attrs e [ThreatModifier 1, AttackModifier 1, DefenseModifier 1]
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      weaker <- defendersWeakerThan eid defenders
      unless (null weaker) $ makeUndefended pid eid
      pure t
    _ -> OrcsOfTheWhiteHand <$> runMessage msg attrs
