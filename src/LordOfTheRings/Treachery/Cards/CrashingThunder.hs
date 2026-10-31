module LordOfTheRings.Treachery.Cards.CrashingThunder (crashingThunder) where

import LordOfTheRings.Trait (Trait (Weather))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CrashingThunder = CrashingThunder TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crashingThunder :: TreacheryCard CrashingThunder
crashingThunder = treachery CrashingThunder Cards.crashingThunder

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the number of quests in play. Exhaust each damaged character in play.
-- Shadow: Shuffle each Weather card from the encounter discard pile into the encounter deck.
instance RunMessage CrashingThunder where
  runMessage msg t@(CrashingThunder attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- questCardsInPlay
      when (x > 0) $ eachPlayer \p -> dealDamageDivided attrs p x
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      for_ damaged exhaust
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      shuffleEncounterDiscardIntoDeck (CardWithTrait Weather)
      pure t
    _ -> CrashingThunder <$> runMessage msg attrs
