module LordOfTheRings.Treachery.Cards.PaleBlade (paleBlade) where

import LordOfTheRings.Trait (Trait (Nazgul))
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PaleBlade = PaleBlade TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge.
paleBlade :: TreacheryCard PaleBlade
paleBlade = treachery PaleBlade Cards.paleBlade

-- | Counts as a Weapon attachment with the text: "Limit 1 per enemy. Attached enemy gets
-- +1 attack and cannot take non-combat damage."
instance HasModifiersFor PaleBlade where
  getModifiersFor (PaleBlade a) = withHost a \host -> modified a host [AttackModifier 1, CannotTakeNonCombatDamage]

-- | When Revealed: Attach to a Nazgûl enemy in play, if able: one without a Pale Blade. The
-- player who revealed it chooses.
instance RunMessage PaleBlade where
  runMessage msg t@(PaleBlade attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      taken <- hostsOfTreachery (toCardCode Cards.paleBlade)
      nazgul <- filter (\e -> not $ any (isTarget e) taken) <$> select (EnemyWithTrait Nazgul)
      chooseOrRunOneM you do
        questionLabeled "paleBlade.attach"
        causedBy attrs
        targets nazgul $ attachTreachery attrs.treacheryId
      pure t
    _ -> PaleBlade <$> runMessage msg attrs
