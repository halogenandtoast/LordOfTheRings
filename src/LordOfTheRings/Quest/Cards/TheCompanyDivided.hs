module LordOfTheRings.Quest.Cards.TheCompanyDivided (theCompanyDivided) where

import LordOfTheRings.Card (Card (EncounterCard))
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (EncounterPhase, RefreshPhase, TravelPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (RingBearer))

newtype TheCompanyDivided = TheCompanyDivided QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCompanyDivided :: QuestCard TheCompanyDivided
theCompanyDivided = quest TheCompanyDivided Cards.theCompanyDivided

{- | Forced: At the end of the refresh phase, shuffle the Seat of Seeing and Frodo's Choice
into the encounter discard pile. Then, place the encounter discard pile on the bottom of the
encounter deck. Starting with the first player, each player creates his own staging area
and advances to a different stage 3A of his choice. In player order, each player moves 1
encounter card from this staging area to his. When there are no encounter cards left at
this stage, discard it. The scenario does it all, keeping track of the cards left.
-}
instance HasAbilities TheCompanyDivided where
  getAbilities (TheCompanyDivided a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.flipped]

{- | When Revealed: Remove the Ring-bearer, and each card attached to it, from the game (treat
The One Ring's text as blank while resolving this effect). Reduce each enemy's engagement
cost to 0 until the end of the engagement phase. Skip the travel phase this round.

Frodo's Choice brings him back ("take control of the Ring-bearer (and each card attached to
it)"), so he waits out of the game as its captive: under no player's control, out of the
game's reach, and, The One Ring's text blank, his leaving no loss (see 'GuardCharacter').
-}
instance RunMessage TheCompanyDivided where
  runMessage msg q@(TheCompanyDivided attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheCompanyDivided <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      choices <- getSetAside (cardIs Objectives.frodosChoice)
      bearers <- select (HeroWithTrait RingBearer)
      for_ (take 1 choices) \choice -> for_ bearers \h -> guardCharacterWith h (EncounterCard choice)
      enemies <- select AnyEnemy
      for_ enemies \e -> do
        cost <- getEngagementCost e
        createEffect attrs e [EngagementCostModifier (negate cost)] (UntilEndOfPhaseOf EncounterPhase)
      roundModifier attrs GameTarget (SkipPhase TravelPhase)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      push $ ScenarioSpecific "breakingOfTheFellowship.divide" Null
      pure q
    _ -> TheCompanyDivided <$> runMessage msg attrs
