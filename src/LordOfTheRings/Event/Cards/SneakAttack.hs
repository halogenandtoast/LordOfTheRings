module LordOfTheRings.Event.Cards.SneakAttack (sneakAttack) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype SneakAttack = SneakAttack EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Put 1 ally card into play from your hand. At the end of the phase, if that
-- ally is still in play, return it to your hand.
sneakAttack :: EventCard SneakAttack
sneakAttack = eventWith SneakAttack Cards.sneakAttack \a -> a & criteriaL .~ CardInHand a.owner AllyType

instance RunMessage SneakAttack where
  runMessage msg e@(SneakAttack attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      player <- getPlayer pid
      allies <- filterM passesUnique [c | c <- player.hand, c.cardType == AllyType]
      let returnLater c = Effect (toSource attrs) (CardIdTarget c.id) [ReturnToHandAtEndOfPhase] UntilEndOfPhase
      unless (null allies)
        $ push
        $ questionLabelWith "Choose an ally to put into play" (Just $ toTarget attrs) [] pid
          [targetLabel (CardIdTarget c.id) [CreateEffect (returnLater c), PutAllyIntoPlay pid c.id] | c <- allies]
      pure e
    _ -> SneakAttack <$> runMessage msg attrs
