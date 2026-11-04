module LordOfTheRings.Enemy.Cards.Hobgoblin (hobgoblin) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (GuardRelease (ReturnToHandWhenFree))

newtype Hobgoblin = Hobgoblin EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

hobgoblin :: EnemyCard Hobgoblin
hobgoblin = enemy Hobgoblin Cards.hobgoblin

-- | While Hobgoblin is guarding a card, it gets +X threat, +X attack and +X defense, where X
-- is that card's cost: its printed cost, 0 for one with none.
instance HasModifiersFor Hobgoblin where
  getModifiersFor (Hobgoblin a) = do
    x <- sum . map printedCostOf <$> playerCardsGuardedBy a.cardId
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x, DefenseModifier x]

-- | Forced: After Hobgoblin enters play, the first player attaches the top card of his deck
-- to Hobgoblin faceup as a guarded attachment. When Hobgoblin leaves play, return the card
-- it was guarding to its owner's hand ('ReturnToHandWhenFree').
instance HasAbilities Hobgoblin where
  getAbilities (Hobgoblin a) = [mkAbility a 1 $ forced $ MatchingEnemyEntersPlayWindow After (EnemyWithId a.id)]

instance RunMessage Hobgoblin where
  runMessage msg e@(Hobgoblin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      deck <- (.deck) <$> getPlayer lead
      for_ [pc | PlayerCard pc <- take 1 deck] \pc -> do
        removeFromDeck lead pc.id
        guardPlayerCard attrs.cardId ReturnToHandWhenFree pc
      pure e
    _ -> Hobgoblin <$> runMessage msg attrs
