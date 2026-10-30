module LordOfTheRings.Treachery.Cards.SuddenStorms (suddenStorms) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SuddenStorms = SuddenStorms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

suddenStorms :: TreacheryCard SuddenStorms
suddenStorms = treachery SuddenStorms Cards.suddenStorms

-- | When Revealed: Either shift your heading off-course or deal 1 damage to each exhausted
-- character. The first player chooses.
-- Shadow: Excess damage from this attack is dealt to a Ship objective you control.
instance RunMessage SuddenStorms where
  runMessage msg t@(SuddenStorms attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      chooseOneM lead do
        questionLabeled "suddenStorms.choose"
        causedBy attrs
        labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse attrs
        labeled "suddenStorms.damage" $ for_ exhausted \c -> dealDamage attrs c 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      attackModifier (CardIdSource cid) eid ExcessDamageToShipObjective
      pure t
    _ -> SuddenStorms <$> runMessage msg attrs
