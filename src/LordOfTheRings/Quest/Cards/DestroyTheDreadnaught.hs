module LordOfTheRings.Quest.Cards.DestroyTheDreadnaught (destroyTheDreadnaught) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types (
  EpicEffect (EpicShuffleWarship),
  EpicTargetQuery (EpicWarshipGroup),
 )
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught.Helpers
import LordOfTheRings.Scenario.Types (scenarioSetAside)

newtype DestroyTheDreadnaught = DestroyTheDreadnaught QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

destroyTheDreadnaught :: QuestCard DestroyTheDreadnaught
destroyTheDreadnaught = quest DestroyTheDreadnaught Cards.destroyTheDreadnaught

instance HasModifiersFor DestroyTheDreadnaught where
  getModifiersFor (DestroyTheDreadnaught a) = do
    players <- initialGroupSize
    modifySelf a [ProgressPerRoundLimit $ 5 * players]

instance HasAbilities DestroyTheDreadnaught where
  getAbilities (DestroyTheDreadnaught a)
    | a.flipped =
        [ anyPlayer
            $ restricted a 1 (EpicTargetsExist EpicWarshipGroup)
            $ ActionAbility
            $ QuestProgressPerPlayerCost a.id 5
        , anyPlayer
            $ mkAbility a 2
            $ ResponseAbility (EnemyAttacksYouWindow When $ EnemyWithTitle "Dreadnaught") (RaiseThreatCost 4)
        ]
    | otherwise = []

instance RunMessage DestroyTheDreadnaught where
  runMessage msg q@(DestroyTheDreadnaught attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DestroyTheDreadnaught <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      cards <- getsGame (scenarioSetAside . toAttrs . gameScenario)
      for_ (take 1 $ filter ((== toCardCode Enemies.dreadnaught) . toCardCode) cards) addToStaging
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      chooseEpicTarget attrs pid EpicWarshipGroup EpicShuffleWarship
      pure q
    UseCardAbility pid (isSource attrs -> True) 2 windows -> do
      for_ [eid | Window When (EnemyAttacking eid who) <- windows, who == pid] cancelAttack
      pure q
    Destroy _ (EnemyTarget eid) -> do
      enemy <- getEnemy eid
      when
        (toCardCode enemy `elem` map toCardCode [Enemies.dreadnaught, Enemies.dreadnaughtDamaged])
        winGame
      DestroyTheDreadnaught <$> runMessage msg attrs
    _ -> DestroyTheDreadnaught <$> runMessage msg attrs
