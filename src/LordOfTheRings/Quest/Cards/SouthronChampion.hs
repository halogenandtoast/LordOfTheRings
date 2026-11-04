module LordOfTheRings.Quest.Cards.SouthronChampion (southronChampion) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Trait (Trait (Harad))

newtype SouthronChampion = SouthronChampion QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Harad Soldiers).
southronChampion :: QuestCard SouthronChampion
southronChampion = quest SouthronChampion Cards.southronChampion

haradEnemy :: CardMatcher
haradEnemy = CardWithType EnemyType <> CardWithTrait Harad

{- | While this stage is in play, the Champion gets +2 threat, +2 attack, +2 defense and
gains: "Immune to player card effects. The main quest stage cannot be defeated while the
Champion is in play." The Champion cannot take damage unless there are at least 4 progress
tokens here. This stage cannot be defeated while the Champion is in play.

The Champion is told by the resource token this stage placed on it, as the players tell
it. Should it leave play the token goes with it, and it is the Champion no longer.
-}
instance HasModifiersFor SouthronChampion where
  getModifiersFor (SouthronChampion a) = do
    champions <- select (EnemyWithTrait Harad <> EnemyWithResources)
    for_ champions \e ->
      modified a e $ [ThreatModifier 2, AttackModifier 2, DefenseModifier 2, ImmuneToPlayerCardEffects] <> [CannotTakeDamage | a.progress < 4]
    unless (null champions) do
      modifySelf a [CannotBeDefeated]
      mains <- getMainQuests
      for_ mains \q -> modified a q [CannotBeDefeated]

-- | When Revealed: Discard cards from the encounter deck until a Harad enemy is discarded.
-- Add that enemy to the staging area and place a resource token on it. That enemy is "the
-- Champion."
instance RunMessage SouthronChampion where
  runMessage msg q@(SouthronChampion attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discardEncounterCardsUntil attrs haradEnemy
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (find (cardMatches haradEnemy) cards) \card -> do
        addToStagingFromDiscard card
        placeResources attrs (CardIdTarget card.id) 1
      pure q
    _ -> SouthronChampion <$> runMessage msg attrs
