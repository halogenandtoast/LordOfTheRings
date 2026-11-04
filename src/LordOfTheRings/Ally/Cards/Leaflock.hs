module LordOfTheRings.Ally.Cards.Leaflock (leaflock) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Ent))

newtype Leaflock = Leaflock AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
leaflock :: AllyCard Leaflock
leaflock = ally (Leaflock . (exhaustedL .~ True)) Cards.leaflock

-- | Cannot have restricted attachments. Leaflock gets +1 willpower for each damaged Ent
-- character you control. (Limit +4 willpower.)
instance HasModifiersFor Leaflock where
  getModifiersFor (Leaflock a) = do
    modifySelf a [CannotHaveRestrictedAttachments]
    for_ (placementController a.placement) \pid -> do
      let you = PlayerWithId pid
      damaged <- damagedCharacters (HeroControlledBy you <> HeroWithTrait Ent) (AllyControlledBy you <> AllyWithTrait Ent)
      let x = min 4 (length damaged)
      when (x > 0) $ modifySelf a [WillpowerModifier x]
