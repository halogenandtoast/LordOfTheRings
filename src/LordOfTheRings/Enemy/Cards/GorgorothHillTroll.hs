module LordOfTheRings.Enemy.Cards.GorgorothHillTroll (gorgorothHillTroll) where

import LordOfTheRings.Card (cdCost, costAsResources, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Player.Types (discardableHand)

newtype GorgorothHillTroll = GorgorothHillTroll EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

gorgorothHillTroll :: EnemyCard GorgorothHillTroll
gorgorothHillTroll = enemy GorgorothHillTroll Cards.gorgorothHillTroll

-- | Cannot have attachments.
instance HasModifiersFor GorgorothHillTroll where
  getModifiersFor (GorgorothHillTroll a) = modifySelf a [CannotHaveAttachments]

-- | Forced: After Gorgoroth Hill-troll attacks and destroys a character, the defending
-- player discards a random card from his hand and raises his threat by that card's printed
-- cost.
instance HasAbilities GorgorothHillTroll where
  getAbilities (GorgorothHillTroll a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage GorgorothHillTroll where
  runMessage msg e@(GorgorothHillTroll attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (nub [pid | Window _ (CharacterDestroyedByAttack _ _ pid) <- ws]) \pid -> do
        hand <- discardableHand <$> getPlayer pid
        unless (null hand) do
          card <- uniform hand
          discardFromHand pid card
          encounterRaisesThreat pid $ maybe 0 costAsResources (cdCost $ toCardDef card)
      pure e
    _ -> GorgorothHillTroll <$> runMessage msg attrs
