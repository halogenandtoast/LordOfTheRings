module LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn (theFortressOfNurn) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Matcher (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheFortressOfNurn = TheFortressOfNurn ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFortressOfNurn :: Scenario
theFortressOfNurn = Scenario $ scenario TheFortressOfNurn "the-fortress-of-nurn" "The Fortress of Nurn"

{- | The Fortress of Nurn, Ulchor's Guard and Under Guard make up the encounter deck; The
Power of Mordor is the Power of Mordor deck, which stage 1A creates (MEC83 rules). Thane
Ulchor and the four Storm the Castle side quests are set aside for stage 1A to add to the
staging area.

The player cards each player places under the quest deck are out of play for the rest of
the game: the scenario keeps them in its meta's "underQuestDeck".
-}
instance RunMessage TheFortressOfNurn where
  runMessage msg (TheFortressOfNurn attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheFortressOfNurn, EncounterSet.UlchorsGuard, EncounterSet.UnderGuard]
      let (aside, deck) =
            partition
              ( cardMatches
                  $ CardMatchAny
                  $ map
                    cardIs
                    [ Enemies.thaneUlchorTheFortressOfNurn
                    , Quests.stormTheCastleCastleGarrison
                    , Quests.stormTheCastleLethalCounterattack
                    , Quests.stormTheCastleNoQuarter
                    , Quests.stormTheCastleStrongholdOfMordor
                    ]
              )
              cards
      firstStage <- genEncounterCard Quests.assaultOnDolRhugar
      finalStage <- genEncounterCard Quests.theFinalConfrontation
      putQuestIntoPlay firstStage
      pure
        $ TheFortressOfNurn
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ aside
        & questDeckL .~ [finalStage]
    PlaceUnderneath ScenarioTarget cards -> do
      let under = case attrs.meta of
            Object o | Just (Array old) <- KeyMap.lookup "underQuestDeck" o -> toList old
            _ -> []
      pure $ TheFortressOfNurn $ attrs & metaL .~ object ["underQuestDeck" .= (under <> map toJSON cards)]
    _ -> TheFortressOfNurn <$> runMessage msg attrs
