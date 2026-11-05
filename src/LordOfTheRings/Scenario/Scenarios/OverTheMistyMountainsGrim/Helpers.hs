{- | What Over the Misty Mountains Grim's cards share: whether The Great Goblin is in the
victory display, the Stone-giants, and the switch to the second encounter deck.
-}
module LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (
  greatGoblinDefeated,
  stoneGiant,
  switchEncounterDeck,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Matcher.Types (EnemyMatcher (EnemyWithCardCode), CardMatcher (CardWithTitle), cardMatches)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (ReverseQueue)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

-- | Whether The Great Goblin is in the victory display.
greatGoblinDefeated :: HasGame m => m Bool
greatGoblinDefeated = getsGame (any (cardMatches $ CardWithTitle "The Great Goblin") . scenarioVictoryDisplay . toAttrs . gameScenario)

stoneGiant :: EnemyMatcher
stoneGiant = EnemyWithCardCode $ toCardCode Enemies.stoneGiant

{- | Down, Down to Goblin Town: the encounter deck, every encounter card in play shuffled
into it, and the discard pile are set aside, inactive, and the second encounter deck
becomes the active one. Nothing makes the first deck active again, so it is removed from
the game (rules insert, "Active and Inactive Encounter Decks").
-}
switchEncounterDeck :: ReverseQueue m => m ()
switchEncounterDeck = push $ ScenarioSpecific "overTheMistyMountainsGrim.switchEncounterDeck" Null
