module LordOfTheRings.Treachery.Cards.DragonBreath (dragonBreath) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Dragon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DragonBreath = DragonBreath TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dragonBreath :: TreacheryCard DragonBreath
dragonBreath = treachery DragonBreath Cards.dragonBreath

-- | Counts as a Condition attachment with the text: "Limit 1 per enemy. Forced: When
-- attached enemy attacks you, deal 1 damage to each character you control. Then, discard
-- Dragon Breath."
instance HasAbilities DragonBreath where
  getAbilities (DragonBreath a) = case a.placement of
    AttachedTo (EnemyTarget eid) -> [mkAbility a 1 $ forced $ EnemyAttacksWindow When eid]
    _ -> []

-- | Surge. When Revealed: Attach to a Dragon enemy, the first player choosing which: one
-- without a copy attached already.
instance RunMessage DragonBreath where
  runMessage msg t@(DragonBreath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      dragons <- map toTarget <$> select (EnemyWithTrait Dragon)
      eligible <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode) dragons
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "dragonBreath.attach"
        causedBy attrs
        targets eligible $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [p | Window _ (EnemyAttacking _ p) <- ws] \p -> do
        characters <- charactersControlledBy p
        for_ characters \c -> dealDamage attrs c 1
      discard attrs attrs
      pure t
    _ -> DragonBreath <$> runMessage msg attrs
