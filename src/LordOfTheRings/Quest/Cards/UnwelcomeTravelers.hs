module LordOfTheRings.Quest.Cards.UnwelcomeTravelers (unwelcomeTravelers) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype UnwelcomeTravelers = UnwelcomeTravelers QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
unwelcomeTravelers :: QuestCard UnwelcomeTravelers
unwelcomeTravelers = quest UnwelcomeTravelers Cards.unwelcomeTravelers

-- | Each location in the staging area gets +1 Threat.
instance HasModifiersFor UnwelcomeTravelers where
  getModifiersFor (UnwelcomeTravelers a) = do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ThreatModifier 1]

-- | Forced: After Unwelcome Travelers becomes the current quest, the first player discards
-- cards from the top of the encounter deck until an enemy is discarded and puts it into
-- play engaged with him.
instance RunMessage UnwelcomeTravelers where
  runMessage msg q@(UnwelcomeTravelers attrs) = case msg of
    SetCurrentQuest (isTarget attrs -> True) -> do
      discardEncounterCardsUntil attrs enemyCard
      UnwelcomeTravelers <$> runMessage msg attrs
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      lead <- getLead
      for_ (find (cardMatches enemyCard) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged lead card
      pure q
    _ -> UnwelcomeTravelers <$> runMessage msg attrs
   where
    enemyCard = CardWithType EnemyType
