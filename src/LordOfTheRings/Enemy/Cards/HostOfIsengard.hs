module LordOfTheRings.Enemy.Cards.HostOfIsengard (hostOfIsengard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype HostOfIsengard = HostOfIsengard EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

hostOfIsengard :: EnemyCard HostOfIsengard
hostOfIsengard = enemy HostOfIsengard Cards.hostOfIsengard

instance HasModifiersFor HostOfIsengard where
  getModifiersFor (HostOfIsengard a) = modifySelf a [CannotHaveAttachments]

instance HasAbilities HostOfIsengard where
  getAbilities (HostOfIsengard a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

instance RunMessage HostOfIsengard where
  runMessage msg e@(HostOfIsengard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgressOnCurrentQuest a 1
      pure e
    _ -> HostOfIsengard <$> runMessage msg a
