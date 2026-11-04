module LordOfTheRings.Treachery.Cards.WrithingShadows (writhingShadows) where

import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers (ashX)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WrithingShadows = WrithingShadows TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

writhingShadows :: TreacheryCard WrithingShadows
writhingShadows = treachery WrithingShadows Cards.writhingShadows

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- 1 more than the number of quest cards in play. Exhaust each character damaged by this
-- effect, read once all of it is dealt, those still in play.
-- Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage WrithingShadows where
  runMessage msg t@(WrithingShadows attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- ashX
      eachPlayer \p -> dealDamageDivided attrs p x
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      inPlay <- selectCharacters AnyHero AnyAlly
      damaged <- filter (`elem` inPlay) . ordNub . map fst . filter ((> 0) . snd) <$> damageDealtBy attrs
      for_ damaged exhaust
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> WrithingShadows <$> runMessage msg attrs
