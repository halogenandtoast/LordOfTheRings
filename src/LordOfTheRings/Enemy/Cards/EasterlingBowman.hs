module LordOfTheRings.Enemy.Cards.EasterlingBowman (easterlingBowman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EasterlingBowman = EasterlingBowman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingBowman :: EnemyCard EasterlingBowman
easterlingBowman = enemy EasterlingBowman Cards.easterlingBowman

-- | When Revealed: Deal 1 damage to a questing character you control.
-- Shadow: After this attack, attacking enemy engages the next player then makes an attack
-- (an immediate attack, dealt a new shadow card).
instance RunMessage EasterlingBowman where
  runMessage msg e@(EasterlingBowman attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      questing <- selectCharacters (HeroControlledBy (PlayerWithId you) <> CommittedHero) (AllyControlledBy (PlayerWithId you) <> CommittedAlly)
      chooseOrRunOneM you do
        questionLabeled "easterlingBowman.damage"
        causedBy attrs
        targets questing \c -> dealDamage attrs c 1
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      getNextPlayer pid >>= traverse_ \next -> do
        again <- capture do
          engageEnemy next eid
          immediateAttack next eid
        afterAttack eid again
      pure e
    _ -> EasterlingBowman <$> runMessage msg attrs
