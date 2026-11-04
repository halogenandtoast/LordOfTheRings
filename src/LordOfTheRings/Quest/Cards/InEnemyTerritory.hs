module LordOfTheRings.Quest.Cards.InEnemyTerritory (inEnemyTerritory) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype InEnemyTerritory = InEnemyTerritory QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest (Harad Territory). Surge, as its card def says.
inEnemyTerritory :: QuestCard InEnemyTerritory
inEnemyTerritory = quest InEnemyTerritory Cards.inEnemyTerritory

-- | Each player cannot have more than 5 cards in his hand. (If you have more than 5,
-- immediately choose and discard cards from hand until you have only 5.)
instance HasModifiersFor InEnemyTerritory where
  getModifiersFor (InEnemyTerritory a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [HandSizeLimit 5]

-- | Response: When this stage is defeated, each player draws a card. The players gain by
-- it, so it is taken, as Make Camp's is.
instance RunMessage InEnemyTerritory where
  runMessage msg (InEnemyTerritory attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> drawCards attrs p 1
      InEnemyTerritory <$> runMessage msg attrs
    _ -> InEnemyTerritory <$> runMessage msg attrs
