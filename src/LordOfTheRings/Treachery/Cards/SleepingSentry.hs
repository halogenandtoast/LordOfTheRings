module LordOfTheRings.Treachery.Cards.SleepingSentry (sleepingSentry) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SleepingSentry = SleepingSentry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sleepingSentry :: TreacheryCard SleepingSentry
sleepingSentry = treachery SleepingSentry Cards.sleepingSentry

-- | When Revealed: Deal 1 damage to each exhausted character. Then, exhaust all ready
-- characters.
-- Shadow: Defending player must discard all exhausted characters he controls.
instance RunMessage SleepingSentry where
  runMessage msg t@(SleepingSentry attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 1
      traverse_ exhaust =<< selectCharacters ReadyHero ReadyAlly
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      let yours = PlayerWithId pid
      traverse_ discard =<< selectCharacters (HeroControlledBy yours <> ExhaustedHero) (AllyControlledBy yours <> ExhaustedAlly)
      pure t
    _ -> SleepingSentry <$> runMessage msg attrs
