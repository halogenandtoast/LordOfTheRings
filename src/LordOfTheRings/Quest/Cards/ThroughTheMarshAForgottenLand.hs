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

-- | A card played from hand marks its player, with an effect of no modifiers that lasts
-- the round.
instance HasAbilities ThroughTheMarshAForgottenLand where
  getAbilities (ThroughTheMarshAForgottenLand a) =
    wanderingStage a <> [mkAbility a 1 $ silent $ CardPlayedWindow After AnyCard | a.flipped]

-- | Each player cannot play more than 1 card each round.
-- ponytail: only cards played from hand while this stage is in play count; one played
-- earlier in the round, or from the discard pile, does not.
instance HasModifiersFor ThroughTheMarshAForgottenLand where
  getModifiersFor (ThroughTheMarshAForgottenLand a) = when a.flipped do
    played <- getsGame \g -> [p | Effect source (PlayerTarget p) _ _ <- gameEffects g, isSource a source]
    for_ (nub played) \p -> modified a (PlayerTarget p) [CannotPlay AnyCard]

-- | When Revealed: Raise each player's threat by 1.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance RunMessage ThroughTheMarshAForgottenLand where
  runMessage msg q@(ThroughTheMarshAForgottenLand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [p | Window _ (PlayedCardFromHand p _) <- ws] \p -> roundModifiers attrs (PlayerTarget p) []
      pure q
    _ -> ThroughTheMarshAForgottenLand <$> runWanderingStage msg attrs
