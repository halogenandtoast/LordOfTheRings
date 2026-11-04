module LordOfTheRings.Quest.Cards.OrcHunting (orcHunting) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers

newtype OrcHunting = OrcHunting QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHunting :: QuestCard OrcHunting
orcHunting = quest OrcHunting Cards.orcHunting

-- | If Frodo's Choice is not in play, any time players would place progress on this quest,
-- discard an equal number of cards from the encounter deck instead.
instance HasModifiersFor OrcHunting where
  getModifiersFor (OrcHunting a) = stageThreeModifiers a

-- | Response: After you quest successfully, discard an enemy engaged with a player.
instance HasAbilities OrcHunting where
  getAbilities (OrcHunting a) = [mkAbility a 1 $ response $ QuestedSuccessfullyWindow After | a.flipped]

-- | Forced: After Frodo's Choice is revealed at this stage, advance to stage 4A. (Side A's
-- Forced effect is the scenario's, as 'SearchingTheWoods' says.)
instance RunMessage OrcHunting where
  runMessage msg q@(OrcHunting attrs) = case msg of
    ProgressCanceled (isSource attrs -> True) n | n > 0 -> do
      discardTopOfEncounterDeck attrs n
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.flipped -> do
      inPlay <- frodosChoiceInPlay
      if inPlay then OrcHunting <$> runMessage msg attrs else q <$ discardTopOfEncounterDeck attrs n
    CheckWindows ws | attrs.flipped -> do
      revealed <- frodosChoiceRevealed ws
      when revealed advanceQuest
      OrcHunting <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "orcHunting.discardEnemy"
        causedBy attrs
        targets enemies discard
      pure q
    _ -> OrcHunting <$> runMessage msg attrs
