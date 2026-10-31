module LordOfTheRings.Ally.Cards.Laermuin (laermuin) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Laermuin = Laermuin AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Loot the Dungeons.
laermuin :: AllyCard Laermuin
laermuin = ally Laermuin Cards.laermuin

-- | Response: After Laermuin commits to a quest, heal 1 damage from a character.
instance HasAbilities Laermuin where
  getAbilities (Laermuin a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage Laermuin where
  runMessage msg s@(Laermuin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- map toTarget <$> select AnyHero
      allies <- map toTarget <$> select AnyAlly
      damaged <- filterM (fmap (> 0) . getDamage) (heroes <> allies)
      chooseOrRunOneM pid do
        questionLabeled "laermuin.heal"
        causedBy a
        targets damaged \t -> healDamage a t 1
      pure s
    _ -> Laermuin <$> runMessage msg a
