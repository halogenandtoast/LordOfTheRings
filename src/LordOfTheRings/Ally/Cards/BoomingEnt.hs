module LordOfTheRings.Ally.Cards.BoomingEnt (boomingEnt) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Ent))

newtype BoomingEnt = BoomingEnt AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
boomingEnt :: AllyCard BoomingEnt
boomingEnt = ally (BoomingEnt . (exhaustedL .~ True)) Cards.boomingEnt

-- | Cannot have restricted attachments. Booming Ent gets +1 attack for each damaged Ent
-- character you control.
instance HasModifiersFor BoomingEnt where
  getModifiersFor (BoomingEnt a) = do
    modifySelf a [CannotHaveRestrictedAttachments]
    for_ (placementController a.placement) \pid -> do
      let you = PlayerWithId pid
      ents <- selectCharacters (HeroControlledBy you <> HeroWithTrait Ent) (AllyControlledBy you <> AllyWithTrait Ent)
      damaged <- length <$> filterM (fmap (> 0) . getDamage) ents
      when (damaged > 0) $ modifySelf a [AttackModifier damaged]
