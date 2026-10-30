module LordOfTheRings.Treachery.Cards.FeebleAndWeary (feebleAndWeary) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FeebleAndWeary = FeebleAndWeary TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

feebleAndWeary :: TreacheryCard FeebleAndWeary
feebleAndWeary = treachery FeebleAndWeary Cards.feebleAndWeary

-- | When Revealed: Each player must either deal 1 damage to each exhausted character he
-- controls, or remove each exhausted character he controls from the quest.
-- Shadow: Exhaust a character you control.
instance RunMessage FeebleAndWeary where
  runMessage msg t@(FeebleAndWeary attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        exhausted <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ExhaustedHero) (AllyControlledBy (PlayerWithId pid) <> ExhaustedAlly)
        unless (null exhausted) $ chooseOneM pid do
          questionLabeled "feebleAndWeary.choose"
          causedBy attrs
          labeled "feebleAndWeary.damage" $ for_ exhausted \c -> dealDamage attrs c 1
          labeled "feebleAndWeary.removeFromQuest" $ for_ exhausted removeFromQuest
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> FeebleAndWeary <$> runMessage msg attrs
