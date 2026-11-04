module LordOfTheRings.Enemy.Cards.NazgulOfMordor (nazgulOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Morgul))

newtype NazgulOfMordor = NazgulOfMordor EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

nazgulOfMordor :: EnemyCard NazgulOfMordor
nazgulOfMordor = enemy NazgulOfMordor Cards.nazgulOfMordor

-- | Cannot have non-Morgul attachments.
instance HasModifiersFor NazgulOfMordor where
  getModifiersFor (NazgulOfMordor a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

-- | Forced: After Nazgûl of Mordor attacks, either raise your threat by 4, or return Nazgûl
-- of Mordor to the staging area.
instance HasAbilities NazgulOfMordor where
  getAbilities (NazgulOfMordor a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage NazgulOfMordor where
  runMessage msg e@(NazgulOfMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid ->
        chooseOneM pid do
          questionLabeled "nazgulOfMordor.choose"
          causedBy attrs
          labeled "nazgulOfMordor.threat" $ encounterRaisesThreat pid 4
          labeled "nazgulOfMordor.return" $ placeEnemy attrs.id StagingArea
      pure e
    _ -> NazgulOfMordor <$> runMessage msg attrs
