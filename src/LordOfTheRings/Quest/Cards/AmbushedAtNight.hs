module LordOfTheRings.Quest.Cards.AmbushedAtNight (ambushedAtNight) where

import LordOfTheRings.Card (genEncounterCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AmbushedAtNight = AmbushedAtNight QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | No quest points: the players advance as they travel to an objective-location.
ambushedAtNight :: QuestCard AmbushedAtNight
ambushedAtNight = quest AmbushedAtNight Cards.ambushedAtNight

-- | Each player's threat cannot be reduced by player card effects.
instance HasModifiersFor AmbushedAtNight where
  getModifiersFor (AmbushedAtNight a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [PlayerCardThreatReductionLimit 0]

-- | Forced: After an enemy engages a player, raise that player's threat by 1.
instance HasAbilities AmbushedAtNight where
  getAbilities (AmbushedAtNight a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]

{- | Setup: Set the Spiders of Mirkwood and Dol Guldur Orcs encounter sets aside, out of
play (the scenario did so as it gathered the encounter deck). The first player takes
control of Haldan and adds Searching for a Way Out to the staging area. Each player adds
1 Forest Spider to the staging area. Shuffle the encounter deck.
-}
instance RunMessage AmbushedAtNight where
  runMessage msg q@(AmbushedAtNight attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      lead <- getLead
      putObjectiveAllyIntoPlayUnder lead =<< genEncounterCard Objectives.haldan
      addToStaging =<< genEncounterCard Objectives.searchingForAWayOut
      players <- getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (take (length players) $ filter ((== toCardCode Enemies.forestSpider) . toCardCode) deck) \c -> do
        removeFromEncounterDeck c
        addToStaging c
      shuffleEncounterDeck
      AmbushedAtNight <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> raiseThreat pid 1
      pure q
    _ -> AmbushedAtNight <$> runMessage msg attrs
