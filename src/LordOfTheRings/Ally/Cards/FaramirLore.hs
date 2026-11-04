module LordOfTheRings.Ally.Cards.FaramirLore (faramirLore) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype FaramirLore = FaramirLore AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

faramirLore :: AllyCard FaramirLore
faramirLore = ally FaramirLore Cards.faramirLore

staged :: EnemyMatcher
staged = EnemyInStagingArea <> NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards

-- | Response: After Faramir enters play, choose a non-unique enemy in the staging area.
-- Engage that enemy and deal 2 damage to it (4 damage instead if it has an attachment on
-- it).
instance HasAbilities FaramirLore where
  getAbilities (FaramirLore a) = [restricted a 1 (exists staged) $ response $ entersPlay a]

instance RunMessage FaramirLore where
  runMessage msg s@(FaramirLore a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select staged
      chooseOrRunOneM pid do
        questionLabeled "faramirLore.engage"
        causedBy a
        targets enemies \e -> do
          attached <- selectAny (EnemyWithId e <> EnemyWithAnyAttachment)
          engageEnemy pid e
          dealDamage a e (if attached then 4 else 2)
      pure s
    _ -> FaramirLore <$> runMessage msg a
