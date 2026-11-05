module LordOfTheRings.Event.Cards.ATaleOfVictories (aTaleOfVictories) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Attachment (heroesWithAttached)
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype ATaleOfVictories = ATaleOfVictories EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A Song boon of the Ered Mithrin campaign.
aTaleOfVictories :: EventCard ATaleOfVictories
aTaleOfVictories = eventWith ATaleOfVictories Cards.aTaleOfVictories (criteriaL .~ DuringPhase CombatPhase)

-- | Combat Action: Draw a card for each enemy engaged with you. Heal 3 damage from the hero
-- with Unbroken attached. The next time that hero would be dealt damage this phase, cancel
-- 3 of that damage.
instance RunMessage ATaleOfVictories where
  runMessage msg e@(ATaleOfVictories attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      engaged <- length <$> select (EnemyEngagedWith (PlayerWithId pid))
      when (engaged > 0) $ drawCards attrs pid engaged
      heroes <- heroesWithAttached Attachments.unbroken
      for_ heroes \h -> do
        healDamage attrs h 3
        phaseModifier attrs h (CancelNextDamage 3)
      pure e
    _ -> ATaleOfVictories <$> runMessage msg attrs
