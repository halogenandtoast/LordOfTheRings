module LordOfTheRings.Event.Cards.KnowledgeOfTheEnemy (knowledgeOfTheEnemy) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype KnowledgeOfTheEnemy = KnowledgeOfTheEnemy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | You can only play 1 Knowledge of the Enemy each round.
knowledgeOfTheEnemy :: EventCard KnowledgeOfTheEnemy
knowledgeOfTheEnemy =
  eventWith KnowledgeOfTheEnemy Cards.knowledgeOfTheEnemy \a ->
    a
      & criteriaL
      .~ ( DuringPhase PlanningPhase
            <> TargetLacksModifier (PlayerTarget a.owner) (PlayedCardThisRound (toCardCode Cards.knowledgeOfTheEnemy) EventType)
         )

-- | Planning Action: Choose a hero you control. Add 1 resource to its resource pool for each
-- enemy engaged with you.
instance RunMessage KnowledgeOfTheEnemy where
  runMessage msg e@(KnowledgeOfTheEnemy attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      n <- length <$> select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "chooseHero"
        causedBy attrs
        targets heroes \h -> when (n > 0) $ placeResources attrs h n
      pure e
    _ -> KnowledgeOfTheEnemy <$> runMessage msg attrs
