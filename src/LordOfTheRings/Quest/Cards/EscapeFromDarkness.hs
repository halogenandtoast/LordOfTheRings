module LordOfTheRings.Quest.Cards.EscapeFromDarkness (escapeFromDarkness) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers
import LordOfTheRings.Trait (Trait (Tools))

newtype EscapeFromDarkness = EscapeFromDarkness QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeFromDarkness :: QuestCard EscapeFromDarkness
escapeFromDarkness = quest EscapeFromDarkness Cards.escapeFromDarkness

-- | Abandoned Tools gains: "Refresh Action: Exhaust attached hero to put a progress token
-- on Escape from Darkness." Its holder may take it, so the stage offers it to them.
instance HasAbilities EscapeFromDarkness where
  getAbilities (EscapeFromDarkness a) =
    searchForAnExit a
      <> bypass a
      <> [ anyPlayer
             $ restricted a 1 (DuringPhase RefreshPhase <> exists toolsHolder)
             $ ActionAbility Free
         | a.flipped
         ]

toolsHolder :: HeroMatcher
toolsHolder = HeroControlledBy ActivePlayer <> ReadyHero <> HeroWithAttached Tools

-- | Progress tokens cannot be placed on Escape from Darkness except by Abandoned Tools. If
-- the players defeat this stage, they escape and win the game.
instance RunMessage EscapeFromDarkness where
  runMessage msg q@(EscapeFromDarkness attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroWithAttached Tools)
      chooseOrRunOneM pid do
        questionLabeled "escapeFromDarkness.exhaust"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          placeProgress tools attrs 1
      pure q
    PlaceTokens source (isTarget attrs -> True) Progress n
      | source /= tools -> pure q
      | attrs.flipped && attrs.progress + n >= attrs.questPoints -> do
          winGame
          pure q
    _ -> EscapeFromDarkness <$> runSearchForAnExit msg attrs
   where
    tools = AbilitySource (toSource attrs) 1
