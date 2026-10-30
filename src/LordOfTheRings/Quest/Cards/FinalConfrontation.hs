module LordOfTheRings.Quest.Cards.FinalConfrontation (finalConfrontation) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Trait (Trait (Raider))

newtype FinalConfrontation = FinalConfrontation QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

finalConfrontation :: QuestCard FinalConfrontation
finalConfrontation = quest FinalConfrontation Cards.finalConfrontation

sahir :: EnemyMatcher
sahir = EnemyWithCardCode (toCardCode Enemies.captainSahirTheCityOfCorsairs)

-- | While another enemy has resources on it, Captain Sahír gains indestructible.
instance HasModifiersFor FinalConfrontation where
  getModifiersFor (FinalConfrontation a) = when a.flipped do
    others <- selectAny (EnemyWithResources <> not_ sahir)
    when others $ traverse_ (\s -> modified a s [GainKeyword Indestructible]) =<< select sahir

{- | When Revealed: Add The Shattered Monument to the staging area. Put Captain Sahír into
play, engaged with the first player. Each player searches the encounter deck and discard
pile for an enemy and adds it to the staging area. Shuffle the encounter deck. Add 1
resource to each Raider enemy in play.
Forced: When a hero or enemy is destroyed, add its resources to Captain Sahír. If Captain
Sahír is destroyed, the players have finally put an end to the Corsairs' treacherous plot
and win the game.
-}
instance RunMessage FinalConfrontation where
  runMessage msg q@(FinalConfrontation attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      FinalConfrontation <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theShatteredMonument)
      setAside' <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      lead <- getLead
      traverse_ (putEnemyIntoPlayEngaged lead) $ take 1 $ filter (cardMatches $ cardIs Enemies.captainSahirTheCityOfCorsairs) setAside'
      eachPlayer \p -> searchEncounterDeckAndDiscard p attrs (CardWithType EnemyType)
      shuffleEncounterDeck
      doStep 1 msg
      pure q
    -- the Raiders counted once the players have added theirs
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      raiders <- select (EnemyWithTrait Raider)
      for_ raiders \r -> placeResources attrs r 1
      pure q
    CheckWindows ws | attrs.flipped -> do
      captain <- selectOne sahir
      when (any ((== toCardCode Enemies.captainSahirTheCityOfCorsairs) . toCardCode) [c | Window After (EnemyDestroyed c) <- ws]) winGame
      for_ captain \s -> do
        for_ [e | Window When (EnemyDefeated e) <- ws, e /= s] \e -> do
          n <- countTokens Resource . enemyTokens . toAttrs <$> getEnemy e
          when (n > 0) $ placeResources attrs s n
        for_ [h | Window When (CharacterDestroyed (HeroTarget h) _) <- ws] \h -> do
          n <- (.resources) <$> getHero h
          when (n > 0) $ placeResources attrs s n
      FinalConfrontation <$> runMessage msg attrs
    _ -> FinalConfrontation <$> runMessage msg attrs
