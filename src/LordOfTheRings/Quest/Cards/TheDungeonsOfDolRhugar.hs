module LordOfTheRings.Quest.Cards.TheDungeonsOfDolRhugar (theDungeonsOfDolRhugar) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castleSideQuests)

newtype TheDungeonsOfDolRhugar = TheDungeonsOfDolRhugar QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge.
theDungeonsOfDolRhugar :: QuestCard TheDungeonsOfDolRhugar
theDungeonsOfDolRhugar = quest TheDungeonsOfDolRhugar Cards.theDungeonsOfDolRhugar

-- | No more than 4 progress can be placed on the current quest each round.
instance HasModifiersFor TheDungeonsOfDolRhugar where
  getModifiersFor (TheDungeonsOfDolRhugar a) = getCurrentQuest >>= traverse_ \q -> modified a q [ProgressPerRoundLimit 4]

{- | Response: When this stage is defeated, the first player chooses a Castle side quest.
Each player looks at his facedown cards under that quest and adds 1 to his hand. The
players gain by it, so it is taken, as Fortify the Defense's is.
-}
instance RunMessage TheDungeonsOfDolRhugar where
  runMessage msg (TheDungeonsOfDolRhugar attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      lead <- getLead
      castles <- castleSideQuests
      chooseOrRunOneM lead do
        questionLabeled "theDungeonsOfDolRhugar.castle"
        causedBy attrs
        targets castles \castle -> do
          under <- getsGame (maybe [] ((.underneath) . toAttrs) . lookup castle . entitiesQuests . gameEntities)
          eachPlayer \p -> do
            let own = [pc | PlayerCard pc <- under, pc.owner == Just p]
            chooseOrRunOneM p do
              questionLabeled "theDungeonsOfDolRhugar.take"
              causedBy attrs
              showing (map PlayerCard own)
              targets own putInHand
      TheDungeonsOfDolRhugar <$> runMessage msg attrs
    _ -> TheDungeonsOfDolRhugar <$> runMessage msg attrs
