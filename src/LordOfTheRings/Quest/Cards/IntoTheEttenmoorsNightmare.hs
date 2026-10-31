module LordOfTheRings.Quest.Cards.IntoTheEttenmoorsNightmare (intoTheEttenmoorsNightmare) where

import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Frontend
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype IntoTheEttenmoorsNightmare = IntoTheEttenmoorsNightmare QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Nightmare Deck's stage 1, in place of the standard one.
intoTheEttenmoorsNightmare :: QuestCard IntoTheEttenmoorsNightmare
intoTheEttenmoorsNightmare = quest IntoTheEttenmoorsNightmare Cards.intoTheEttenmoorsNightmare

-- | Forced: At the beginning of the quest phase, if there are no encounter side quests in
-- play, discard cards from the top of the encounter deck until a side quest is discarded,
-- reveal it, and add it to the staging area.
instance HasAbilities IntoTheEttenmoorsNightmare where
  getAbilities (IntoTheEttenmoorsNightmare a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase]

-- | Setup: Set aside each side quest and each location with the safe keyword. Reveal 1 of
-- those side quests and 1 of those safe locations at random, adding them to the staging
-- area with the safe location guarded by the side quest. Shuffle the remaining side quests
-- and safe locations into the encounter deck. Discard cards from the top of the encounter
-- deck until X enemies are discarded, where X is the number of players in the game. Add the
-- discarded enemies to the staging area. The first player gains control of Amarthiúl.
-- The others never leave the encounter deck, which is shuffled before the discards.
instance RunMessage IntoTheEttenmoorsNightmare where
  runMessage msg q@(IntoTheEttenmoorsNightmare attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let sideQuests = sideQuestsAmong deck
          safe = filter isSafeCard deck
      unless (null sideQuests) do
        sideQuest <- uniform sideQuests
        removeFromEncounterDeck sideQuest
        addToStaging sideQuest
        unless (null safe) do
          location <- uniform safe
          removeFromEncounterDeck location
          putLocationIntoPlay location (AttachedTo $ CardIdTarget sideQuest.id)
      -- Amarthiúl joins the first player as he comes out
      for_ (take 1 $ filter (cardMatches $ cardIs Objectives.amarthiulAcrossTheEttenmoors) deck) \card -> do
        removeFromEncounterDeck card
        addToStaging card
      shuffleEncounterDeck
      doStep 1 msg
      pure $ IntoTheEttenmoorsNightmare $ attrs {questFlipped = True}
    DoStep 1 (Flip (isTarget attrs -> True)) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      players <- length <$> getRemainingPlayers
      let enemies = [i | (i, c) <- zip [1 ..] deck, cdCardType (toCardDef c) == EnemyType]
      discardTopOfEncounterDeckFor attrs GameTarget $ fromMaybe (length deck) (headMay $ drop (players - 1) enemies)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) GameTarget cards -> do
      traverse_ addToStagingFromDiscard [c | c <- cards, cdCardType (toCardDef c) == EnemyType]
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      none <- null <$> getEncounterSideQuests
      when none $ discardUntilSideQuest attrs
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ revealFromEncounterDiscard (sideQuestsAmong cards)
      pure q
    _ -> IntoTheEttenmoorsNightmare <$> runMessage msg attrs
