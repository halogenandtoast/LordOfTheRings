module LordOfTheRings.Quest.Cards.WarriorsOfTheEast (warriorsOfTheEast) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Easterling))

newtype WarriorsOfTheEast = WarriorsOfTheEast QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
warriorsOfTheEast :: QuestCard WarriorsOfTheEast
warriorsOfTheEast = quest WarriorsOfTheEast Cards.warriorsOfTheEast

-- | Surge.
-- Forced: After an Easterling enemy enters play, attach the top Easterling treachery in the
-- discard pile to that enemy. (Hall of Beorn prints it "When Revealed:"; the card reads
-- "Forced:".) Several entering at once take one each, from the top down.
instance HasAbilities WarriorsOfTheEast where
  getAbilities (WarriorsOfTheEast a) = [mkAbility a 1 $ forced $ MatchingEnemyEntersPlayWindow After (EnemyWithTrait Easterling)]

-- | Forced: After Warriors of the East becomes the current quest, the highest Attack
-- Easterling enemy in the staging area attacks the first player, the first player choosing
-- among those tied.
instance RunMessage WarriorsOfTheEast where
  runMessage msg q@(WarriorsOfTheEast attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      tops <- filter (cardMatches easterlingTreachery) <$> getEncounterDiscard
      for_ (zip [eid | Window _ (EnemyEnteredPlay eid) <- ws] tops) \(eid, card) -> attachFromEncounterDiscard card eid
      pure q
    SetCurrentQuest (isTarget attrs -> True) -> do
      staged <- select (EnemyWithTrait Easterling <> EnemyInStagingArea)
      attacks <- for staged \e -> (e,) <$> getEnemyAttack e
      lead <- getLead
      for_ (maximumMay $ map snd attacks) \highest ->
        chooseOrRunOneM lead do
          questionLabeled "warriorsOfTheEast.attack"
          causedBy attrs
          targets [e | (e, n) <- attacks, n == highest] (immediateAttack lead)
      WarriorsOfTheEast <$> runMessage msg attrs
    _ -> WarriorsOfTheEast <$> runMessage msg attrs
