module LordOfTheRings.Treachery.Cards.PoisonedBySpiders (poisonedBySpiders) where

import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn, poisonedCharacters)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PoisonedBySpiders = PoisonedBySpiders TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

poisonedBySpiders :: TreacheryCard PoisonedBySpiders
poisonedBySpiders = treachery PoisonedBySpiders Cards.poisonedBySpiders

-- | When Revealed: Each character takes damage equal to the number of poison it has. If no
-- characters were damaged by this effect, this card gains surge.
-- Shadow: Discard an unconscious character you control.
instance RunMessage PoisonedBySpiders where
  runMessage msg t@(PoisonedBySpiders attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      poisoned <- poisonedCharacters
      for_ poisoned \c -> dealDamage attrs c =<< poisonOn c
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      damaged <- filter ((> 0) . snd) <$> damageDealtBy attrs
      when (null damaged) revealEncounterCard
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      unconscious <- selectCharacters (UnconsciousHero <> HeroControlledBy (PlayerWithId pid)) (UnconsciousAlly <> AllyControlledBy (PlayerWithId pid))
      chooseOrRunOneM pid do
        questionLabeled "poisonedBySpiders.discard"
        causedBy attrs
        targets unconscious discard
      pure t
    _ -> PoisonedBySpiders <$> runMessage msg attrs
