module LordOfTheRings.Game.Public (publicGame) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base
import LordOfTheRings.Card
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Id
import LordOfTheRings.Target
import LordOfTheRings.Prelude

-- | The game as the players may see it: decks are hidden information, so only their
-- sizes are sent. Stored games and exports keep everything. The quest tally is added
-- so the board shows the engine's totals, modifiers included, as are the hosts each
-- attachment in hand could go on, so dragging one only lights up those.
publicGame :: Game -> Value
publicGame g = hideDecks $ toJSON g
 where
  hideDecks = \case
    Object o ->
      Object
        $ KeyMap.insert "committedWillpower" (toJSON $ runReadGame getCommittedWillpower g)
        $ KeyMap.insert "stagingThreat" (toJSON $ runReadGame getStagingThreat g)
        $ KeyMap.insert "attachmentHosts" (toJSON attachHosts)
        $ adjust (overObjects hidePlayer) "players"
        $ adjust hideScenario "scenario" o
    v -> v
  attachHosts :: Map CardId [Target]
  attachHosts =
    mapFromList
      [ (c.id, runReadGame (attachmentHosts $ attachMatcherFor $ toCardCode c) g)
      | p <- toList g.players
      , c <- p.hand
      , c.cardType == AttachmentType
      ]
  hidePlayer = withSize "deck" "deckSize"
  hideScenario = withSize "questDeck" "questDeckSize" . withSize "encounterDeck" "encounterDeckSize"
  withSize key sizeKey = \case
    Object o | Just (Array cards) <- KeyMap.lookup key o ->
      Object $ KeyMap.insert sizeKey (toJSON $ length cards) $ KeyMap.delete key o
    v -> v
  adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)
  overObjects f = \case
    Object o -> Object $ fmap f o
    v -> v
