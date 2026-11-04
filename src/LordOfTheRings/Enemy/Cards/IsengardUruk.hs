module LordOfTheRings.Enemy.Cards.IsengardUruk (isengardUruk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype IsengardUruk = IsengardUruk EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Toughness 1. Archery 2, as its card def says.
isengardUruk :: EnemyCard IsengardUruk
isengardUruk = enemy IsengardUruk Cards.isengardUruk

{- | When Revealed: Either assign 3 damage among characters you control, or reveal an
additional encounter card. The damage is offered only to a player who controls a
character, so it can be done in full (FAQ 1.44).
-}
instance RunMessage IsengardUruk where
  runMessage msg e@(IsengardUruk attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      characters <- charactersControlledBy you
      chooseOrRunOneM you do
        questionLabeled "isengardUruk.choose"
        causedBy attrs
        when (notNull characters) $ labeled "isengardUruk.damage" $ dealDamageDividedAmong attrs you characters 3
        labeled "isengardUruk.reveal" revealEncounterCard
      pure e
    _ -> IsengardUruk <$> runMessage msg attrs
