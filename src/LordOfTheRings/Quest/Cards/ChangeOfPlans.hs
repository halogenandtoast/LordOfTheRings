module LordOfTheRings.Quest.Cards.ChangeOfPlans (changeOfPlans) where

import LordOfTheRings.Card (cdEncounterSet, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet (EncounterSet (CorsairRaiders, TheThingInTheDepths, TheThingInTheDepthsNightmare))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

newtype ChangeOfPlans = ChangeOfPlans QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

changeOfPlans :: QuestCard ChangeOfPlans
changeOfPlans = quest ChangeOfPlans Cards.changeOfPlans

-- | When Revealed: Flip Captain Sahír and Na'asiyah to their objective sides, removing all
-- tokens from them. Search the encounter deck, discard pile and all play areas for each
-- encounter card from the Corsair Raiders encounter set and remove each of those cards
-- from the game. Add The Thing in the Depths to the staging area. Shuffle the encounter
-- discard pile and the set aside The Thing in the Depths encounter set into the encounter
-- deck. Advance to stage 3.
instance RunMessage ChangeOfPlans where
  runMessage msg q@(ChangeOfPlans attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      ChangeOfPlans <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      elite <- concatMapM (select . EnemyWithCardCode . toCardCode) [Enemies.captainSahir, Enemies.naasiyah]
      for_ elite (flipOver . EnemyTarget)
      let inSet set = (== Just set) . cdEncounterSet . toCardDef
          -- Nightmare: Corsair Duelist is of the Corsair Raiders set too
          raider c = inSet CorsairRaiders c || toCardCode c == toCardCode Enemies.corsairDuelist
      s <- getsGame (toAttrs . gameScenario)
      for_ (filter raider s.scenarioEncounterDeck) removeFromEncounterDeck
      for_ (filter raider s.scenarioEncounterDiscard) removeFromEncounterDiscard
      raiders <- filterM (fmap raider . getEnemy) =<< select AnyEnemy
      for_ raiders (removeFromGame . EnemyTarget)
      addSetAsideToStaging (cardIs Enemies.theThingInTheDepths)
      shuffleEncounterDiscardIntoDeck AnyCard
      shuffleSetAsideIntoEncounterDeck
        [ c | c <- s.scenarioSetAside, inSet TheThingInTheDepths c || inSet TheThingInTheDepthsNightmare c, toCardCode c /= toCardCode Enemies.theThingInTheDepths
        ]
      completeQuest attrs.id
      pure q
    _ -> ChangeOfPlans <$> runMessage msg attrs
