module LordOfTheRings.Treachery.Cards.DragonScales (dragonScales) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DragonScales = DragonScales TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dragonScales :: TreacheryCard DragonScales
dragonScales = treachery DragonScales Cards.dragonScales

-- | Counts as a Condition attachment with the text: "Limit 1 per enemy. Forced: When
-- attached enemy would take any amount of damage, cancel all of that damage. Then, discard
-- Dragon Scales."
instance HasAbilities DragonScales where
  getAbilities (DragonScales a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> [mkAbility a 1 $ forced $ DealtDamageWindow When host]
    _ -> []

-- | Surge. When Revealed: Attach to a Dragon enemy, the first player choosing which: one
-- without a copy attached already.
instance RunMessage DragonScales where
  runMessage msg t@(DragonScales attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      dragons <- map toTarget <$> select (EnemyWithTrait Dragon)
      eligible <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode) dragons
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "dragonScales.attach"
        causedBy attrs
        targets eligible $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs cancelDamage
      discard attrs attrs
      pure t
    _ -> DragonScales <$> runMessage msg attrs
