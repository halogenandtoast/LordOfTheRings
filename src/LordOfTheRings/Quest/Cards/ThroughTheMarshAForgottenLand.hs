module LordOfTheRings.Quest.Cards.ThroughTheMarshAForgottenLand (throughTheMarshAForgottenLand) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameEffects)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype ThroughTheMarshAForgottenLand = ThroughTheMarshAForgottenLand QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMarshAForgottenLand :: QuestCard ThroughTheMarshAForgottenLand
throughTheMarshAForgottenLand = quest ThroughTheMarshAForgottenLand Cards.throughTheMarshAForgottenLand

instance HasAbilities ThroughTheMarshAForgottenLand where
  getAbilities (ThroughTheMarshAForgottenLand a) = wanderingStage a

-- | Each player cannot play more than 1 card each round: any card he played this round,
-- from wherever, before this stage too, counts.
instance HasModifiersFor ThroughTheMarshAForgottenLand where
  getModifiersFor (ThroughTheMarshAForgottenLand a) = when a.flipped do
    played <- getsGame \g -> [p | Effect _ (PlayerTarget p) ms _ <- gameEffects g, PlayedACardThisRound `elem` ms]
    for_ (nub played) \p -> modified a (PlayerTarget p) [CannotPlay AnyCard]

-- | When Revealed: Raise each player's threat by 1.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance RunMessage ThroughTheMarshAForgottenLand where
  runMessage msg q@(ThroughTheMarshAForgottenLand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    _ -> ThroughTheMarshAForgottenLand <$> runWanderingStage msg attrs
