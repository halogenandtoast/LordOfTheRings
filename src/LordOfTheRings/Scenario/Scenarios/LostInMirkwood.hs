module LordOfTheRings.Scenario.Scenarios.LostInMirkwood (lostInMirkwood) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EredMithrinCampaign (eredMithrinAbilities, eredMithrinModifiers, runEredMithrinCampaign)
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (nextStage)

newtype LostInMirkwood = LostInMirkwood ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInMirkwood :: Scenario
lostInMirkwood = Scenario $ scenario LostInMirkwood "lost-in-mirkwood" "Lost in Mirkwood"

-- | The Ered Mithrin campaign card's abilities and modifiers, in campaign mode.
instance HasAbilities LostInMirkwood where
  getAbilities (LostInMirkwood a) = eredMithrinAbilities a

instance HasModifiersFor LostInMirkwood where
  getModifiersFor (LostInMirkwood a) = eredMithrinModifiers a

{- | The encounter deck is built from Lost in Mirkwood, Dark Woods, Gathering Gloom,
Passage Through Mirkwood, Spiders of Mirkwood and Dol Guldur Orcs (MEC65 rules); stage
1A's setup sets the last two aside, done here as the deck is gathered. The quest deck
holds both versions of stages 2 to 4: an objective-location says which the players
advance to, and the other version of that stage leaves the quest deck.
-}
instance RunMessage LostInMirkwood where
  runMessage msg (LostInMirkwood attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [ EncounterSet.LostInMirkwood
          , EncounterSet.DarkWoods
          , EncounterSet.GatheringGloom
          , EncounterSet.PassageThroughMirkwood
          , EncounterSet.SpidersOfMirkwood
          , EncounterSet.DolGuldurOrcs
          ]
      let asideSet c = cdEncounterSet (toCardDef c) `elem` map Just [EncounterSet.SpidersOfMirkwood, EncounterSet.DolGuldurOrcs]
          (aside, encounterDeck) = partition asideSet cards
      firstStage <- genEncounterCard Quests.ambushedAtNight
      laterStages <-
        traverse
          genEncounterCard
          [ Quests.spidersOfMirkwoodStage
          , Quests.dolGuldurOrcsStage
          , Quests.carriedAway
          , Quests.trulyLost
          , Quests.theForestOfGreatFear
          , Quests.escapeFromTaurNuFuin
          ]
      putQuestIntoPlay firstStage
      pure
        $ LostInMirkwood
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ aside
    -- "advance to the next stage A (or C), if able": the stage is left, not defeated, and
    -- the next is the version named; the other version of it is out of the game
    ScenarioSpecific "lostInMirkwood.advance" (parseMaybe parseJSON -> Just sideA) -> do
      mains <- getsGame (filter (not . isSideQuest) . toList . (.entities.quests))
      case mains of
        current : _
          | Just stage <- cdStage (toCardDef current)
          , Just next <- nextStage sideA stage attrs.questDeck -> do
              -- what happens "when the players advance from this stage" first
              push $ ScenarioSpecific "lostInMirkwood.advancing" Null
              push $ CompleteQuest (toId current)
              let rest = filter ((/= cdStage (toCardDef next)) . cdStage . toCardDef) attrs.questDeck
              pure $ LostInMirkwood $ attrs & questDeckL .~ (next : rest)
        _ -> pure $ LostInMirkwood attrs
    ScenarioSpecific "lostInMirkwood.nemesis" v -> pure $ LostInMirkwood $ attrs {scenarioMeta = v}
    _ -> LostInMirkwood <$> runEredMithrinCampaign msg attrs
