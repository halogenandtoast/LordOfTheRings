module LordOfTheRings.Enemy.Cards.CorsairSkiff (corsairSkiff) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CorsairSkiff = CorsairSkiff EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairSkiff :: EnemyCard CorsairSkiff
corsairSkiff = enemy CorsairSkiff Cards.corsairSkiff

-- | Surge. Cannot have attachments.
instance HasModifiersFor CorsairSkiff where
  getModifiersFor (CorsairSkiff a) = modifySelf a [CannotHaveAttachments]

-- | Forced: When Corsair Skiff engages you from the staging area, either raise your threat
-- by 3 or it gains Boarding 1.
instance HasAbilities CorsairSkiff where
  getAbilities (CorsairSkiff a) = [restricted a 1 (exists $ EnemyWithId a.id <> EnemyInStagingArea) $ forced $ EnemyEngagedWindow When a.id]

-- | Boarding resolves once it has engaged, so the Boarding it gains is read then.
instance RunMessage CorsairSkiff where
  runMessage msg e@(CorsairSkiff attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "corsairSkiff.choose"
          causedBy attrs
          labeled "corsairSkiff.threat" $ encounterRaisesThreat pid 3
          labeled "corsairSkiff.boarding" $ roundModifier attrs attrs (BoardingModifier 1)
      pure e
    _ -> CorsairSkiff <$> runMessage msg attrs
