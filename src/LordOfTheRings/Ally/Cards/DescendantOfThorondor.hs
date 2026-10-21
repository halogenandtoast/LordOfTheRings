module LordOfTheRings.Ally.Cards.DescendantOfThorondor (descendantOfThorondor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

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
    [restricted a 1 (exists EnemyInStagingArea) $ response $ entersPlay a]

-- | Leaving play, it is asked as it goes, while it can still answer.
instance RunMessage DescendantOfThorondor where
  runMessage msg s@(DescendantOfThorondor a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      strike pid True
      pure s
    _ | leaving msg, Just pid <- placementController a.placement -> do
      strike pid False
      DescendantOfThorondor <$> runMessage msg a
    _ -> DescendantOfThorondor <$> runMessage msg a
   where
    leaving = \case
      Discard t -> isTarget a t
      ReturnToHand t -> isTarget a t
      ShuffleIntoOwnersDeck t -> isTarget a t
      _ -> False
    strike pid entering = do
      enemies <- select EnemyInStagingArea
      chooseOneM pid do
        questionLabeled "descendantOfThorondor.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 2
        unless entering $ labeled "pass" $ pure ()
