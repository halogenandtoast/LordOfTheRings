module LordOfTheRings.Ally.Cards.Arador (arador) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (captiveLeavesPlay, runCaptive)

-- | Arador, a Captive objective ally of The Land of Sorrow (see 'runCaptive').
newtype Arador = Arador AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

arador :: AllyCard Arador
arador = ally Arador Cards.arador

-- | Immune to player card effects.
instance HasModifiersFor Arador where
  getModifiersFor (Arador a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Response: After Arador is declared as a defender, ready him. (Limit once per round.)
-- If Arador leaves play, the players lose the game.
instance HasAbilities Arador where
  getAbilities (Arador a) =
    [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ response $ DefenderDeclaredWindow After (Just $ toTarget a)
    , captiveLeavesPlay a
    ]

instance RunMessage Arador where
  runMessage msg s@(Arador a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      ready a
      pure s
    _ -> Arador <$> runCaptive msg a
