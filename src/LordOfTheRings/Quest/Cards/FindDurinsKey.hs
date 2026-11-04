module LordOfTheRings.Quest.Cards.FindDurinsKey (findDurinsKey) where

import LordOfTheRings.Card (toCard, toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion.Helpers (firstPlayerControls, guardOf, searchAndGuard)

newtype FindDurinsKey = FindDurinsKey QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

findDurinsKey :: QuestCard FindDurinsKey
findDurinsKey = quest FindDurinsKey Cards.findDurinsKey

-- | Each encounter card guarding a card is immune to player card effects.
-- The players cannot travel to the Deep Ravine guarding Durin's Key unless there is at
-- least 5 progress on this stage.
instance HasModifiersFor FindDurinsKey where
  getModifiersFor (FindDurinsKey a) = when a.flipped do
    enemies <- select EnemyGuardingACard
    for_ enemies \e -> modified a e [ImmuneToPlayerCardEffects]
    locations <- select LocationGuardingACard
    for_ locations \l -> modified a l [ImmuneToPlayerCardEffects]
    when (a.progress < 5) do
      guard' <- guardOf Objectives.durinsKey
      ravines <- getsGame (filter ((== guard') . Just . (.id) . toCard) . toList . (.entities.locations))
      for_ ravines \l -> modified a l [CannotBeTraveledTo]

{- | This stage cannot be defeated unless the players control Urdug and Durin's Key: Urdug
under the first player, the only one who may control him, and the Key on a hero (Hall of
Beorn's text; the community card list reads "the first player controls").
-}
defeatable :: HasGame m => m Bool
defeatable = do
  urdug <- firstPlayerControls Objectives.urdug
  durinsKeys <- getsGame (filter ((== toCardCode Objectives.durinsKey) . toCardCode) . toList . (.entities.objectives))
  pure $ urdug && or [True | k <- durinsKeys, AttachedTo (HeroTarget _) <- [k.placement]]

{- | When Revealed: The first player searches the encounter deck, discard pile, and victory
display for a Deep Ravine, adds it to the staging area, and attaches the set-aside Durin's
Key to it as a guarded objective. Shuffle the encounter deck. Each other player reveals an
encounter card (as stages 2A all read; Hall of Beorn's "each player" is taken for a typo).
Forced: When the players defeat this stage, remove it from the game. Advance to a random
stage 2A, if able. Otherwise, advance to stage 3A (the scenario draws it).
-}
instance RunMessage FindDurinsKey where
  runMessage msg q@(FindDurinsKey attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FindDurinsKey <$> runConditionalStage defeatable msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchAndGuard Locations.deepRavine Objectives.durinsKey
      pure q
    RescueCharacter {} -> recheck
    SetFirstPlayer {} -> recheck
    _ -> FindDurinsKey <$> runConditionalStage defeatable msg attrs
   where
    recheck = do
      checkStage attrs.id
      FindDurinsKey <$> runConditionalStage defeatable msg attrs
