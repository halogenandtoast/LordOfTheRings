module LordOfTheRings.Ally.Cards.Edrahil (edrahil) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow.Helpers (captiveLeavesPlay, runCaptive)

-- | Edrahil, a Captive objective ally of The Land of Sorrow (see 'runCaptive').
newtype Edrahil = Edrahil AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

edrahil :: AllyCard Edrahil
edrahil = ally Edrahil Cards.edrahil

-- | Immune to player card effects.
instance HasModifiersFor Edrahil where
  getModifiersFor (Edrahil a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Response: After Edrahil quests successfully, choose a player to draw 1 card.
-- If Edrahil leaves play, the players lose the game.
instance HasAbilities Edrahil where
  getAbilities (Edrahil a) =
    [ restricted a 1 (exists $ AllyWithId a.id <> CommittedAlly) $ response $ QuestedSuccessfullyWindow After
    , captiveLeavesPlay a
    ]

instance RunMessage Edrahil where
  runMessage msg s@(Edrahil a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      choosePlayerM pid a \p -> drawCards a p 1
      pure s
    _ -> Edrahil <$> runCaptive msg a
