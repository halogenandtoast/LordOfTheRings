module LordOfTheRings.Event.Cards.DawnTakeYouAll (dawnTakeYouAll) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype DawnTakeYouAll = DawnTakeYouAll EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play after shadow cards have been dealt, before any attacks have resolved.
-- Combat Action: Each player may choose and discard 1 facedown shadow card from an enemy
-- with which he is engaged.
dawnTakeYouAll :: EventCard DawnTakeYouAll
dawnTakeYouAll = eventWith DawnTakeYouAll Cards.dawnTakeYouAll (criteriaL .~ DuringPhase CombatPhase)

instance RunMessage DawnTakeYouAll where
  runMessage msg e@(DawnTakeYouAll attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      eachPlayer \p -> do
        enemies <- filter (notNull . (.shadowCards)) <$> (traverse getEnemy =<< select (EnemyEngagedWith $ PlayerWithId p))
        chooseOneM p do
          questionLabeled "dawnTakeYouAll.discard"
          causedBy attrs
          for_ enemies \en -> for_ (take 1 en.shadowCards) \s ->
            targeting en $ discardShadowCard en.id s
          labeled "pass" $ pure ()
      pure e
    _ -> DawnTakeYouAll <$> runMessage msg attrs
