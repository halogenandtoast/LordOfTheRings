module LordOfTheRings.Ally.Cards.Farin (farin) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (captiveLeavesPlay, runCaptive)

-- | Farin, a Captive objective ally of The Land of Sorrow (see 'runCaptive').
newtype Farin = Farin AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

farin :: AllyCard Farin
farin = ally Farin Cards.farin

-- | Immune to player card effects.
instance HasModifiersFor Farin where
  getModifiersFor (Farin a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Response: After Farin is declared as an attacker against an enemy, deal 1 damage to an
-- enemy engaged with a player.
-- If Farin leaves play, the players lose the game.
instance HasAbilities Farin where
  getAbilities (Farin a) =
    [ restricted a 1 (exists $ EnemyEngagedWith AnyPlayer) $ response $ AttackerDeclaredWindow After (Just $ toTarget a)
    , captiveLeavesPlay a
    ]

instance RunMessage Farin where
  runMessage msg s@(Farin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "farin.damage"
        causedBy a
        targets enemies \e -> dealDamage a e 1
      pure s
    _ -> Farin <$> runCaptive msg a
