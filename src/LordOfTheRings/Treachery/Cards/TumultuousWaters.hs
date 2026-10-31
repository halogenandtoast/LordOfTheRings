module LordOfTheRings.Treachery.Cards.TumultuousWaters (tumultuousWaters) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TumultuousWaters = TumultuousWaters TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tumultuousWaters :: TreacheryCard TumultuousWaters
tumultuousWaters = treachery TumultuousWaters Cards.tumultuousWaters

-- | When Revealed: Exhaust each character with 1 or less willpower. Remove all resources
-- from each ready hero and objective-ally.
-- Shadow: If the defending character has 1 or less willpower, treat this attack as
-- undefended.
instance RunMessage TumultuousWaters where
  runMessage msg t@(TumultuousWaters attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      weak <- filterM (fmap (<= 1) . getWillpower) characters
      for_ weak exhaust
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      heroes <- traverse getHero =<< select ReadyHero
      for_ heroes \h -> when (h.resources > 0) $ removeTokens attrs h.id Resource h.resources
      allies <- traverse getAlly =<< select (ObjectiveAlly <> ReadyAlly)
      for_ allies \x -> when (x.resources > 0) $ removeTokens attrs x.id Resource x.resources
      pure t
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      weak <- filterM (fmap (<= 1) . getWillpower) defenders
      when (notNull defenders && length weak == length defenders) $ makeUndefended pid eid
      pure t
    _ -> TumultuousWaters <$> runMessage msg attrs
