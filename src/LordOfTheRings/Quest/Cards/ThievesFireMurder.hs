module LordOfTheRings.Quest.Cards.ThievesFireMurder (thievesFireMurder) where

import LordOfTheRings.Card (Card (EncounterCard), genEncounterCard, toCard, toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (enemyProgress, getSmaugTheGolden, getSmaugTheMagnificent)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioQuestDeck))

newtype ThievesFireMurder = ThievesFireMurder QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thievesFireMurder :: QuestCard ThievesFireMurder
thievesFireMurder = quest ThievesFireMurder Cards.thievesFireMurder

{- | When Revealed: Remove all progress from Smaug the Golden and set him aside, out of
play. Then, add Smaug the Magnificent to the staging area.

Forced: After placing the 7th progress token on this stage, remove all damage from Smaug
the Magnificent and set him aside, out of play. Then, add Smaug the Golden to the staging
area and reset the quest deck to stage 2B: stage 2B in play, stages 3 and 4 the quest
deck below it.
-}
instance RunMessage ThievesFireMurder where
  runMessage msg q@(ThievesFireMurder attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      ThievesFireMurder <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      golden <- getSmaugTheGolden
      for_ golden \sg -> do
        progress <- enemyProgress sg
        when (progress > 0) $ removeTokens attrs sg Progress progress
        setAsideEnemy sg
      addSetAsideToStaging $ cardIs Enemies.smaugTheMagnificent
      pure q
    PlaceTokens _ (isTarget attrs -> True) Progress n | attrs.progress < 7 && attrs.progress + n >= 7 -> do
      magnificent <- getSmaugTheMagnificent
      for_ magnificent \sm -> do
        damage <- getDamage (EnemyTarget sm)
        when (damage > 0) $ removeTokens attrs sm Damage damage
        setAsideEnemy sm
      addSetAsideToStaging $ cardIs Enemies.smaugTheGolden
      stage2 <- genEncounterCard Cards.theBurglarsTurnTheLonelyMountain
      deck <- getsGame (scenarioQuestDeck . toAttrs . (.scenario))
      case toCard attrs of
        EncounterCard stage4 -> resetToStage stage2 (filter ((/= toCardCode stage4) . toCardCode) deck <> [stage4])
        _ -> pure ()
      -- it leaves play, not defeated: no check of its quest points follows
      pure $ ThievesFireMurder $ attrs & tokensL %~ addTokens Progress n
    _ -> ThievesFireMurder <$> runMessage msg attrs
