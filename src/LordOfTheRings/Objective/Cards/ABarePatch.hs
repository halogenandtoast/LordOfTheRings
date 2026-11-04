module LordOfTheRings.Objective.Cards.ABarePatch (aBarePatch) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (getSmaugTheMagnificent, makeBurgleAttempt)

newtype ABarePatch = ABarePatch ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

aBarePatch :: ObjectiveCard ABarePatch
aBarePatch = objective ABarePatch Cards.aBarePatch

-- | While attached, Smaug the Magnificent gets -3 defense.
instance HasModifiersFor ABarePatch where
  getModifiersFor (ABarePatch a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> modified a host [DefenseModifier (-3)]
    _ -> pure ()

-- | Response: After the players quest successfully, the first player makes a burgle
-- attempt. Once attached, it has nothing left to attempt.
instance HasAbilities ABarePatch where
  getAbilities (ABarePatch a) =
    [ restricted a 1 (exists $ EnemyWithCardCode $ toCardCode Enemies.smaugTheMagnificent) $ response $ QuestedSuccessfullyWindow After
    | not (attached a.placement)
    ]
   where
    attached = \case
      AttachedTo _ -> True
      _ -> False

-- | If successful, attach A Bare Patch to Smaug the Magnificent.
instance RunMessage ABarePatch where
  runMessage msg o@(ABarePatch attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      smaug <- getSmaugTheMagnificent
      makeBurgleAttempt attrs (traverse_ (attachObjective attrs.id) smaug) (pure ())
      pure o
    _ -> ABarePatch <$> runMessage msg attrs
