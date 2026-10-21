module LordOfTheRings.Ally.Cards.DescendantOfThorondor (descendantOfThorondor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DescendantOfThorondor = DescendantOfThorondor AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

descendantOfThorondor :: AllyCard DescendantOfThorondor
descendantOfThorondor = ally DescendantOfThorondor Cards.descendantOfThorondor

-- | Descendant of Thorondor cannot have restricted attachments.
-- Response: After Descendant of Thorondor enters or leaves play, deal 2 damage to any 1
-- enemy in the staging area.
instance HasAbilities DescendantOfThorondor where
  getAbilities (DescendantOfThorondor a) =
    [ restricted a 1 (exists EnemyInStagingArea) $ response $ entersPlay a
    , restricted a 2 (exists EnemyInStagingArea) $ response $ LeftPlayWindow After (toTarget a)
    ]

instance RunMessage DescendantOfThorondor where
  runMessage msg s@(DescendantOfThorondor a) = case msg of
    UseCardAbility pid (isSource a -> True) n _ | n `elem` [1, 2] -> do
      enemies <- select EnemyInStagingArea
      chooseOneM pid do
        questionLabeled "descendantOfThorondor.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 2
      pure s
    _ -> DescendantOfThorondor <$> runMessage msg a
