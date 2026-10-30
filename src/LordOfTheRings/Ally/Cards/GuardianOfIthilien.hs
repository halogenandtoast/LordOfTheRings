module LordOfTheRings.Ally.Cards.GuardianOfIthilien (guardianOfIthilien) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea), placementController)

newtype GuardianOfIthilien = GuardianOfIthilien AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardianOfIthilien :: AllyCard GuardianOfIthilien
guardianOfIthilien = ally GuardianOfIthilien Cards.guardianOfIthilien

-- | Response: After Guardian of Ithilien enters play, return an enemy engaged with you to
-- the staging area.
instance HasAbilities GuardianOfIthilien where
  getAbilities (GuardianOfIthilien a) =
    [ restricted a 1 (exists $ EnemyEngagedWith $ PlayerWithId you) $ response $ entersPlay a
    | Just you <- [placementController a.placement]
    ]

instance RunMessage GuardianOfIthilien where
  runMessage msg s@(GuardianOfIthilien a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy a
        targets enemies \eid -> placeEnemy eid StagingArea
      pure s
    _ -> GuardianOfIthilien <$> runMessage msg a
