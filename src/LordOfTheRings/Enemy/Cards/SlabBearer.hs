module LordOfTheRings.Enemy.Cards.SlabBearer (slabBearer) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SlabBearer = SlabBearer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

slabBearer :: EnemyCard SlabBearer
slabBearer = enemy SlabBearer Cards.slabBearer

instance HasModifiersFor SlabBearer where
  getModifiersFor (SlabBearer a) = modifySelf a [CannotHaveAttachments]

instance HasAbilities SlabBearer where
  getAbilities (SlabBearer a) = [mkAbility a 1 $ forced $ ThisEnemyDefeatedWindow When a.id]

instance RunMessage SlabBearer where
  runMessage msg e@(SlabBearer a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discardTopOfEncounterDeck a 1
      pure e
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      for_ cards \c -> when (cdCardType (toCardDef c) == EnemyType) $ addToStagingFromDiscard c
      pure e
    _ -> SlabBearer <$> runMessage msg a
