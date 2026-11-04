module LordOfTheRings.Quest.Cards.RallyTheFighters (rallyTheFighters) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RallyTheFighters = RallyTheFighters QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
rallyTheFighters :: QuestCard RallyTheFighters
rallyTheFighters = quest RallyTheFighters Cards.rallyTheFighters

-- | Shadow cards are immune to player card effects.
-- A player card canceling a shadow effect is one of those effects.
instance HasModifiersFor RallyTheFighters where
  getModifiersFor (RallyTheFighters a) = do
    enemies <- getsGame (toList . (.entities.enemies))
    for_ [c | e <- enemies, c <- e.shadowCards] \c ->
      modified a (CardIdTarget c.id) [ImmuneToPlayerCardEffects, ShadowCannotBeCanceled]

-- | Response: After this quest is defeated, discard 1 shadow card from each enemy in play.
-- The players gain by it, so it is taken.
instance RunMessage RallyTheFighters where
  runMessage msg (RallyTheFighters attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      enemies <- traverse getEnemy =<< select AnyEnemy
      for_ enemies \e -> for_ (take 1 e.shadowCards) (discardShadowCard e.id)
      RallyTheFighters <$> runMessage msg attrs
    _ -> RallyTheFighters <$> runMessage msg attrs
