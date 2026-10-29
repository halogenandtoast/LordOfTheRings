module LordOfTheRings.Quest.Cards.ThroughTheMarshNoEndInSight (throughTheMarshNoEndInSight) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph.Helpers (runWanderingStage, wanderingStage)

newtype ThroughTheMarshNoEndInSight = ThroughTheMarshNoEndInSight QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMarshNoEndInSight :: QuestCard ThroughTheMarshNoEndInSight
throughTheMarshNoEndInSight = quest ThroughTheMarshNoEndInSight Cards.throughTheMarshNoEndInSight

instance HasAbilities ThroughTheMarshNoEndInSight where
  getAbilities (ThroughTheMarshNoEndInSight a) = wanderingStage a

-- | Player card effects cannot be used to gain resources or draw cards.
-- ponytail: encounter card effects are barred too; none in this encounter deck gives
-- resources or draws.
instance HasModifiersFor ThroughTheMarshNoEndInSight where
  getModifiersFor (ThroughTheMarshNoEndInSight a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotDrawCardsFromCardEffects]
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [CannotGainResourcesFromCardEffects]

-- | When Revealed: Raise each player's threat by 1.
-- Forced: When the players defeat this stage, advance to a random stage 3A (the scenario
-- picks it).
instance RunMessage ThroughTheMarshNoEndInSight where
  runMessage msg q@(ThroughTheMarshNoEndInSight attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure q
    _ -> ThroughTheMarshNoEndInSight <$> runWanderingStage msg attrs
