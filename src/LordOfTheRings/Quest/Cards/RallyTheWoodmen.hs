module LordOfTheRings.Quest.Cards.RallyTheWoodmen (rallyTheWoodmen) where

import LordOfTheRings.Card (CardType (AllyType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RallyTheWoodmen = RallyTheWoodmen QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
rallyTheWoodmen :: QuestCard RallyTheWoodmen
rallyTheWoodmen = quest RallyTheWoodmen Cards.rallyTheWoodmen

-- | Allies cannot be declared as attackers against Dagnir.
instance HasModifiersFor RallyTheWoodmen where
  getModifiersFor (RallyTheWoodmen a) = do
    allies <- select AnyAlly
    for_ allies \ally -> modified a ally [CannotAttackMatching (EnemyWithCardCode $ toCardCode Enemies.dagnir)]

-- | Response: When Rally the Woodmen is defeated, each player searches the top 5 cards of
-- his deck for an ally, puts it into play and shuffles his deck. The players gain by it,
-- so it is taken, as Seek Shade's is.
instance RunMessage RallyTheWoodmen where
  runMessage msg (RallyTheWoodmen attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      eachPlayer \p -> searchDeckIntoPlay p attrs 5 (CardWithType AllyType) 1 []
      RallyTheWoodmen <$> runMessage msg attrs
    _ -> RallyTheWoodmen <$> runMessage msg attrs
