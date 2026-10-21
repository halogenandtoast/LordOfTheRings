module LordOfTheRings.Ally.Cards.WestfoldLancer (westfoldLancer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WestfoldLancer = WestfoldLancer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

westfoldLancer :: AllyCard WestfoldLancer
westfoldLancer = ally WestfoldLancer Cards.westfoldLancer

-- | Response: After Westfold Lancer quests successfully, discard it to choose a
-- non-unique enemy in the staging area. Deal 2 damage to the chosen enemy.
instance HasAbilities WestfoldLancer where
  getAbilities (WestfoldLancer a) =
    [ restricted a 1 (exists (AllyWithId a.id <> CommittedAlly) <> exists targets')
        $ response
        $ QuestedSuccessfullyWindow After
    ]

targets' :: EnemyMatcher
targets' = EnemyInStagingArea <> not_ UniqueEnemy

instance RunMessage WestfoldLancer where
  runMessage msg s@(WestfoldLancer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select targets'
      chooseOrRunOneM pid do
        questionLabeled "westfoldLancer.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 2
      -- the discard is the cost, but what it leads to waits for the effect
      discard a
      pure s
    _ -> WestfoldLancer <$> runMessage msg a
