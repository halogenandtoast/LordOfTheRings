module LordOfTheRings.Quest.Cards.BattleInTheCamp (battleInTheCamp) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (haradObjectivesControlledBy, haradStage, revealRandomHaradObjective)

newtype BattleInTheCamp = BattleInTheCamp QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleInTheCamp :: QuestCard BattleInTheCamp
battleInTheCamp = quest BattleInTheCamp Cards.battleInTheCamp

-- | If a Harad objective is not attached, the first player claims it and attaches it to a
-- hero he controls. The Black Serpent is considered to be engaged with each player who
-- controls a Harad objective. This stage cannot be defeated unless each Harad objective in
-- play is attached to a hero.
instance HasModifiersFor BattleInTheCamp where
  getModifiersFor (BattleInTheCamp a) = when a.flipped do
    haradStage a
    players <- filterM (fmap notNull . haradObjectivesControlledBy) =<< getRemainingPlayers
    serpents <- select (EnemyWithCardCode $ toCardCode Enemies.theBlackSerpent)
    for_ serpents \s -> modified a s [EngagedWithEachPlayer (PlayerWithId p) | p <- players]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck. The first
-- player randomly selects 1 set aside Harad objective, reveals it, and adds it to the
-- staging area.
instance RunMessage BattleInTheCamp where
  runMessage msg q@(BattleInTheCamp attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      BattleInTheCamp <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      revealRandomHaradObjective
      pure q
    _ -> BattleInTheCamp <$> runMessage msg attrs
