module LordOfTheRings.Enemy.Cards.AngmarCaptain (angmarCaptain) where

import LordOfTheRings.Card (CardType (AllyType), cdCardType, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype AngmarCaptain = AngmarCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

angmarCaptain :: EnemyCard AngmarCaptain
angmarCaptain = enemy AngmarCaptain Cards.angmarCaptain

-- | Forced: After Angmar Captain attacks and destroys an ally, discard the top card of the
-- defending player's deck. If the discarded card is an ally, Angmar Captain makes an
-- additional attack against the defending player.
instance HasAbilities AngmarCaptain where
  getAbilities (AngmarCaptain a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | The defending player is the one whose ally the attack destroyed.
instance RunMessage AngmarCaptain where
  runMessage msg e@(AngmarCaptain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (CharacterDestroyedByAttack _ (AllyTarget _) pid) <- ws]) \pid -> do
        top <- take 1 . (.deck) <$> getPlayer pid
        discardTopOfDeck pid 1
        when (any ((== AllyType) . cdCardType . toCardDef) top) $ additionalAttack pid attrs.id
      pure e
    _ -> AngmarCaptain <$> runMessage msg attrs
