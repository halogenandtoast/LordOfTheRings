module LordOfTheRings.Enemy.Cards.GreyMountainGiant (greyMountainGiant) where

import LordOfTheRings.Card (CardType (AllyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GreyMountainGiant = GreyMountainGiant EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

greyMountainGiant :: EnemyCard GreyMountainGiant
greyMountainGiant = enemy GreyMountainGiant Cards.greyMountainGiant

-- | Cannot have non-objective attachments.
instance HasModifiersFor GreyMountainGiant where
  getModifiersFor (GreyMountainGiant a) = modifySelf a [CannotHaveAttachments]

-- | Forced: When Grey Mountain Giant engages you, discard the top 5 cards of your deck.
-- Assign damage among characters you control equal to the number of allies discarded by
-- this effect.
instance HasAbilities GreyMountainGiant where
  getAbilities (GreyMountainGiant a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage GreyMountainGiant where
  runMessage msg e@(GreyMountainGiant attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid -> do
        top <- take 5 . (.deck) <$> getPlayer pid
        discardTopOfDeck pid 5
        let allies = length $ filter ((== AllyType) . cdCardType . toCardDef) top
        when (allies > 0) $ dealDamageDivided attrs pid allies
      pure e
    _ -> GreyMountainGiant <$> runMessage msg attrs
