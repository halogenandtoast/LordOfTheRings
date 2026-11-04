module LordOfTheRings.Quest.Cards.TheSeatOfAmonHen (theSeatOfAmonHen) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers

newtype TheSeatOfAmonHen = TheSeatOfAmonHen QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theSeatOfAmonHen :: QuestCard TheSeatOfAmonHen
theSeatOfAmonHen = quest TheSeatOfAmonHen Cards.theSeatOfAmonHen

-- | If Frodo's Choice is not in play, any time players would place progress on this quest,
-- discard an equal number of cards from the encounter deck instead.
instance HasModifiersFor TheSeatOfAmonHen where
  getModifiersFor (TheSeatOfAmonHen a) = stageThreeModifiers a

-- | Response: After you quest successfully, choose a player. That player draws 2 cards and
-- lowers his threat by 2.
instance HasAbilities TheSeatOfAmonHen where
  getAbilities (TheSeatOfAmonHen a) = [mkAbility a 1 $ response $ QuestedSuccessfullyWindow After | a.flipped]

-- | Forced: After Frodo's Choice is revealed at this stage, advance to stage 4A. (Side A's
-- Forced effect is the scenario's, as 'SearchingTheWoods' says.)
instance RunMessage TheSeatOfAmonHen where
  runMessage msg q@(TheSeatOfAmonHen attrs) = case msg of
    ProgressCanceled (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeck attrs n
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      inPlay <- frodosChoiceInPlay
      if inPlay then TheSeatOfAmonHen <$> runMessage msg attrs else q <$ discardTopOfEncounterDeck attrs n
    CheckWindows ws | attrs.flipped -> do
      revealed <- frodosChoiceRevealed ws
      when revealed advanceQuest
      TheSeatOfAmonHen <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "theSeatOfAmonHen.choosePlayer"
        causedBy attrs
        targets players \p -> do
          drawCards attrs p 2
          lowerThreatBy attrs p 2
      pure q
    _ -> TheSeatOfAmonHen <$> runMessage msg attrs
