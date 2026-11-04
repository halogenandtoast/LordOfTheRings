module LordOfTheRings.Quest.Cards.GuardTheHobbits (guardTheHobbits) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers

newtype GuardTheHobbits = GuardTheHobbits QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

guardTheHobbits :: QuestCard GuardTheHobbits
guardTheHobbits = quest GuardTheHobbits Cards.guardTheHobbits

-- | If Frodo's Choice is not in play, any time players would place progress on this quest,
-- discard an equal number of cards from the encounter deck instead.
instance HasModifiersFor GuardTheHobbits where
  getModifiersFor (GuardTheHobbits a) = stageThreeModifiers a

-- | Response: After you quest successfully, choose a player. Ready each of that player's
-- heroes.
instance HasAbilities GuardTheHobbits where
  getAbilities (GuardTheHobbits a) = [mkAbility a 1 $ response $ QuestedSuccessfullyWindow After | a.flipped]

-- | Forced: After Frodo's Choice is revealed at this stage, advance to stage 4A. (Side A's
-- Forced effect is the scenario's, as 'SearchingTheWoods' says.)
instance RunMessage GuardTheHobbits where
  runMessage msg q@(GuardTheHobbits attrs) = case msg of
    ProgressCanceled (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeck attrs n
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      inPlay <- frodosChoiceInPlay
      if inPlay then GuardTheHobbits <$> runMessage msg attrs else q <$ discardTopOfEncounterDeck attrs n
    CheckWindows ws | attrs.flipped -> do
      revealed <- frodosChoiceRevealed ws
      when revealed advanceQuest
      GuardTheHobbits <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      chooseOrRunOneM pid do
        questionLabeled "guardTheHobbits.choosePlayer"
        causedBy attrs
        targets players \p -> traverse_ ready =<< select (HeroControlledBy $ PlayerWithId p)
      pure q
    _ -> GuardTheHobbits <$> runMessage msg attrs
