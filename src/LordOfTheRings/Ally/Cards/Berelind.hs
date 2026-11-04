module LordOfTheRings.Ally.Cards.Berelind (berelind) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Berelind = Berelind AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Loot the Dungeons.
berelind :: AllyCard Berelind
berelind = ally Berelind Cards.berelind

-- | Response: After Berelind attacks and destroys an enemy, place 1 progress on a quest
-- card in play.
instance HasAbilities Berelind where
  getAbilities (Berelind a) = [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just $ toTarget a)]

instance RunMessage Berelind where
  runMessage msg s@(Berelind a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      placeProgressOnAQuestCard pid a 1
      pure s
    _ -> Berelind <$> runMessage msg a
