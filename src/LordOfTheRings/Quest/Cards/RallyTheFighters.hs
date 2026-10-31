module LordOfTheRings.Quest.Cards.RallyTheFighters (rallyTheFighters) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype RallyTheFighters = RallyTheFighters QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest. Surge, as its card def says.
rallyTheFighters :: QuestCard RallyTheFighters
rallyTheFighters = quest RallyTheFighters Cards.rallyTheFighters

-- | Shadow cards are immune to player card effects.
-- ponytail: no encounter card effect can be canceled, which is what stops a player card
-- canceling a shadow effect.
instance HasModifiersFor RallyTheFighters where
  getModifiersFor (RallyTheFighters a) = modified a GameTarget [CannotCancelEncounterEffects]

-- | Response: After this quest is defeated, discard 1 shadow card from each enemy in play.
-- The players gain by it, so it is taken.
instance RunMessage RallyTheFighters where
  runMessage msg (RallyTheFighters attrs) = case msg of
    CompleteQuest qid | qid == attrs.id -> do
      enemies <- traverse getEnemy =<< select AnyEnemy
      for_ enemies \e -> for_ (take 1 e.shadowCards) (discardShadowCard e.id)
      RallyTheFighters <$> runMessage msg attrs
    _ -> RallyTheFighters <$> runMessage msg attrs
