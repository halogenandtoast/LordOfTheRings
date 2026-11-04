module LordOfTheRings.Treachery.Cards.SwordOfRhun (swordOfRhun) where

import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Easterling, Weapon))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SwordOfRhun = SwordOfRhun TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

swordOfRhun :: TreacheryCard SwordOfRhun
swordOfRhun = treachery SwordOfRhun Cards.swordOfRhun

-- | While attached to an enemy, counts as a Weapon attachment with the text: "Attached
-- enemy gets +2 Attack."
instance HasModifiersFor SwordOfRhun where
  getModifiersFor (SwordOfRhun a) = case a.placement of
    AttachedTo host@(EnemyTarget _) -> do
      modifySelf a [GainTrait Weapon]
      modified a host [AttackModifier 2]
    _ -> pure ()

-- | Surge.
-- When Revealed: Attach to the Easterling enemy with the lowest Attack without a Sword of
-- Rhûn.
instance RunMessage SwordOfRhun where
  runMessage msg t@(SwordOfRhun attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      void $ attachToEnemyWith attrs attrs.treacheryId attrs.treacheryCardCode (EnemyWithTrait Easterling) Lowest getEnemyAttack (const $ pure ())
      pure t
    _ -> SwordOfRhun <$> runMessage msg attrs
