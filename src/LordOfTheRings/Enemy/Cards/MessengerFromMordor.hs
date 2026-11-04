module LordOfTheRings.Enemy.Cards.MessengerFromMordor (messengerFromMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationThreat)

newtype MessengerFromMordor = MessengerFromMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

messengerFromMordor :: EnemyCard MessengerFromMordor
messengerFromMordor = enemy MessengerFromMordor Cards.messengerFromMordor

-- | When Revealed: Either raise each player's threat by X, where X is the active location's
-- Threat, or Messenger from Mordor makes an immediate attack against the first player. The
-- first player chooses.
-- Shadow: Attacking enemy gets +1 Attack.
instance RunMessage MessengerFromMordor where
  runMessage msg e@(MessengerFromMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- maybe (pure 0) getLocationThreat . headMay =<< select IsActiveLocation
      lead <- getLead
      chooseOneM lead do
        questionLabeled "messengerFromMordor.choose"
        causedBy attrs
        labeledWith "messengerFromMordor.raiseThreat" [intVar "n" x] $ eachPlayer \p -> encounterRaisesThreat p x
        labeled "messengerFromMordor.attack" $ immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> MessengerFromMordor <$> runMessage msg attrs
