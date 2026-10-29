module LordOfTheRings.Event.Cards.CloseCall (closeCall) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype CloseCall = CloseCall EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Doomed X. Response: Cancel X damage just dealt to a hero.
--
-- X is chosen as the event resolves, up to the damage just dealt; its card def carries
-- Doomed 0 for the keyword alone, and the threat is raised here.
closeCall :: EventCard CloseCall
closeCall = eventWith CloseCall Cards.closeCall \a -> a & windowL ?~ HeroDealtDamageWindow After

instance RunMessage CloseCall where
  runMessage msg e@(CloseCall attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      for_ (take 1 [(h, n) | Window _ (DealtDamage h@(HeroTarget _) n) <- windows]) \(h, n) -> do
        damage <- getDamage h
        chooseOneM pid do
          questionLabeled "closeCall.cancel"
          causedBy attrs
          aimedAt [h]
          for_ [1 .. min n damage] \x -> labeledWith "xEquals" [intVar "x" x] do
            raiseThreatFromDoomed x
            removeTokens attrs h Damage x
      pure e
    _ -> CloseCall <$> runMessage msg attrs
