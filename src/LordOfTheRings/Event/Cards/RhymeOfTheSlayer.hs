module LordOfTheRings.Event.Cards.RhymeOfTheSlayer (rhymeOfTheSlayer) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (heroesWithAttached)
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype RhymeOfTheSlayer = RhymeOfTheSlayer EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A Song boon of the Ered Mithrin campaign.
rhymeOfTheSlayer :: EventCard RhymeOfTheSlayer
rhymeOfTheSlayer =
  eventWith RhymeOfTheSlayer Cards.rhymeOfTheSlayer (criteriaL .~ (DuringPhase CombatPhase <> exists EnemyNotImmuneToPlayerCards))

-- | Combat Action: Choose an enemy. That enemy gets -4 [attack] until the end of the phase.
-- The hero with Dragon-slayer attached gets +4 [attack] while attacking that enemy this
-- phase.
instance RunMessage RhymeOfTheSlayer where
  runMessage msg e@(RhymeOfTheSlayer attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select EnemyNotImmuneToPlayerCards
      slayers <- heroesWithAttached Attachments.dragonSlayer
      chooseOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \eid -> do
          phaseModifier attrs eid (AttackModifier (-4))
          for_ slayers \h -> phaseModifier attrs h (AttackBonusAgainstMatching (EnemyWithId eid) 4)
      pure e
    _ -> RhymeOfTheSlayer <$> runMessage msg attrs
