module LordOfTheRings.Quest.Cards.OnTheTrail (onTheTrail) where

import LordOfTheRings.Card (EncounterCard (..), genEncounterCard, toCardCode)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Clue))

newtype OnTheTrail = OnTheTrail QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

onTheTrail :: QuestCard OnTheTrail
onTheTrail = quest OnTheTrail Cards.onTheTrail

-- | Any player who does not control a hero with at least 1 Clue objective attached cannot
-- commit characters to this quest.
instance HasModifiersFor OnTheTrail where
  getModifiersFor (OnTheTrail a) = do
    players <- getRemainingPlayers
    for_ players \p -> do
      clue <- selectAny (HeroControlledBy (PlayerWithId p) <> HeroWithAttached Clue)
      unless clue $ modified a (PlayerTarget p) [CannotCommitCharacters]

-- | If there are ever no heroes with Clue objectives attached in play, reset the quest
-- deck to stage 2B. If the players defeat this stage, they have won the game.
instance RunMessage OnTheTrail where
  runMessage msg q@(OnTheTrail attrs) = case msg of
    CheckStage qid | qid == attrs.id -> do
      none <- selectNone (HeroWithAttached Clue)
      when none do
        stage2 <- genEncounterCard Cards.aNewTerrorAbroad
        push $ ResetToStage stage2 [toEncounterCard attrs]
      pure q
    Flip (isTarget attrs -> True) -> recheck
    ReturnToTopOfEncounterDeck _ -> recheck
    ShuffleIntoEncounterDeck _ -> recheck
    PlaceObjective {} -> recheck
    Discard _ -> recheck
    _ -> OnTheTrail <$> runMessage msg attrs
   where
    recheck = do
      push $ CheckStage attrs.id
      OnTheTrail <$> runMessage msg attrs
    toEncounterCard a = MkEncounterCard a.cardId (toCardCode a)
