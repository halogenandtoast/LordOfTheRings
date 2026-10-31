module LordOfTheRings.Location.Cards.EphelDuath (ephelDuath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)
import LordOfTheRings.Trait (Trait (Captain))

newtype EphelDuath = EphelDuath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ephelDuath :: LocationCard EphelDuath
ephelDuath = location EphelDuath Cards.ephelDuath

-- | While Ephel Dúath is in the staging area, each Captain enemy gets +2 attack.
instance HasModifiersFor EphelDuath where
  getModifiersFor (EphelDuath a) = when (a.placement == StagingArea) do
    captains <- select (EnemyWithTrait Captain)
    for_ captains \c -> modified a c [AttackModifier 2]

-- | Shadow: Defending player must either place 1 progress token on To the Tower or treat
-- this attack as undefended. An undefended attack already is, so it asks nothing.
instance RunMessage EphelDuath where
  runMessage msg l@(EphelDuath a) = case msg of
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "ephelDuath.choose"
          causedBy a
          labeled "morgulSorcerer.progress" $ placeOnTower a 1
          labeled "morgulSorcerer.undefended" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> EphelDuath <$> runMessage msg a
