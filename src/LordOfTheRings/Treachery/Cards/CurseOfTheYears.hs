module LordOfTheRings.Treachery.Cards.CurseOfTheYears (curseOfTheYears) where

import LordOfTheRings.Attachment.Types (AttachmentAttrs (attachmentExhausted))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur.Helpers (controlsClue)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CurseOfTheYears = CurseOfTheYears TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

curseOfTheYears :: TreacheryCard CurseOfTheYears
curseOfTheYears = treachery CurseOfTheYears Cards.curseOfTheYears

-- | When Revealed: Deal 1 damage to each exhausted character. Each player with a threat of
-- 35 or higher must also discard each exhausted attachment he controls.
-- Shadow: Attacking enemy gets +1 attack (+3 attack instead if the defending player does
-- not control at least 1 Clue objective).
instance RunMessage CurseOfTheYears where
  runMessage msg t@(CurseOfTheYears attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 1
      attachments <- getsGame (toList . (.attachments) . (.entities))
      eachPlayer \p -> do
        threat <- (.threat) <$> getPlayer p
        when (threat >= 35)
          $ for_ [a | a <- attachments, (toAttrs a).controller == p, attachmentExhausted (toAttrs a)] (discard attrs)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      clue <- controlsClue pid
      enemyGetsAttack (CardIdSource cid) eid (if clue then 1 else 3)
      pure t
    _ -> CurseOfTheYears <$> runMessage msg attrs
