module LordOfTheRings.Quest.Cards.GollumsBetrayal (gollumsBetrayal) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (cardInVictoryDisplay, getShelob, placeResourceOnShelob, shelob)
import LordOfTheRings.Scenario.Scenarios.ThePassageOfTheMarshes.Helpers (smeagol)

newtype GollumsBetrayal = GollumsBetrayal QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

gollumsBetrayal :: QuestCard GollumsBetrayal
gollumsBetrayal = quest GollumsBetrayal Cards.gollumsBetrayal

gollum :: EnemyMatcher
gollum = EnemyWithCardCode (toCardCode Enemies.gollumTheLandOfShadow)

-- | If Gollum is defeated, add him to the victory display. (Do not flip him over.)
instance HasModifiersFor GollumsBetrayal where
  getModifiersFor (GollumsBetrayal a) = when a.flipped do
    traverse_ (\g -> modified a g [CannotBeFlipped]) =<< select gollum

-- | If Gollum is defeated, add him to the victory display.
-- While Shelob has at least 1 resource on her, she gains: "Forced: When Shelob is dealt
-- any amount of damage, cancel that damage and discard 1 resource from Shelob instead."
instance HasAbilities GollumsBetrayal where
  getAbilities (GollumsBetrayal a)
    | a.flipped =
        [ mkAbility a 1 $ forced $ MatchingEnemyDefeatedWindow After gollum
        , mkAbility a 2 $ forced $ MatchingEnemyDealtDamageWindow When (shelob <> EnemyWithResources)
        ]
    | otherwise = []

shelobAndGollumDefeated :: QueueT Message GameT Bool
shelobAndGollumDefeated = (&&) <$> cardInVictoryDisplay Enemies.shelob <*> cardInVictoryDisplay Enemies.gollumTheLandOfShadow

{- | When Revealed: Add Cleft of Cirith Ungol to the staging area. Flip Sméagol to Gollum and
heal all damage from Shelob. Each player places 1 resource on Shelob.

The players cannot defeat this stage unless Shelob and Gollum are in the victory display.
If the players defeat this stage, they win the game: it is the last in the quest deck.
-}
instance RunMessage GollumsBetrayal where
  runMessage msg q@(GollumsBetrayal attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      GollumsBetrayal <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.cleftOfCirithUngol)
      traverse_ flipOver =<< select smeagol
      getShelob >>= traverse_ (healAllDamage attrs)
      eachPlayer \_ -> placeResourceOnShelob attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyDefeated eid) <- ws] addToVictoryDisplay
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 ws -> do
      for_ [eid | Window _ (DealtDamage (EnemyTarget eid) _) <- ws] \eid -> do
        cancelDamage eid
        removeTokens attrs eid Resource 1
      pure q
    _ -> GollumsBetrayal <$> runConditionalStage shelobAndGollumDefeated msg attrs
