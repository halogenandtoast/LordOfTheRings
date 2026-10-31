module LordOfTheRings.Quest.Cards.MakeAStand (makeAStand) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (flipToNight, getTimeObjective, isNight)

newtype MakeAStand = MakeAStand QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest, with no quest points.
makeAStand :: QuestCard MakeAStand
makeAStand = quest MakeAStand Cards.makeAStand

-- | The Time objective cannot flip to Daybreak.
instance HasModifiersFor MakeAStand where
  getModifiersFor (MakeAStand a) = whenM isNight do
    time <- getTimeObjective
    for_ time \o -> modified a o [CannotBeFlipped]

-- | Response: After an enemy is defeated, place 1 resource token on Make a Stand.
instance HasAbilities MakeAStand where
  getAbilities (MakeAStand a) = [mkAbility a 1 $ response $ EnemyDefeatedWindow After]

-- | When Revealed: If it is Day, flip the Time objective to Nightfall.
-- Then, if there are 2 resource tokens per player on Make a Stand, or if there are no
-- enemies in play, add Make a Stand to the victory display.
instance RunMessage MakeAStand where
  runMessage msg q@(MakeAStand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      flipToNight
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeTokens attrs attrs Resource 1
      players <- length <$> getRemainingPlayers
      noEnemies <- not <$> selectAny AnyEnemy
      when (countTokens Resource attrs.questTokens + 1 >= 2 * players || noEnemies) $ addToVictoryDisplay attrs
      pure q
    _ -> MakeAStand <$> runMessage msg attrs
