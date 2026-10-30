module LordOfTheRings.Attachment.Cards.HeirOfValandil (heirOfValandil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Trait (Trait (Dunedain))

newtype HeirOfValandil = HeirOfValandil AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dúnedain hero.
heirOfValandil :: AttachmentCard HeirOfValandil
heirOfValandil = attachment HeirOfValandil Cards.heirOfValandil & attachTo (AttachToHero $ HeroWithTrait Dunedain)

-- | Planning Action: Exhaust Heir of Valandil to reduce the cost of the next Dúnedain ally
-- you play this phase by 1 for each enemy engaged with you.
instance HasAbilities HeirOfValandil where
  getAbilities (HeirOfValandil a) = [restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility (exhaustCost a)]

instance RunMessage HeirOfValandil where
  runMessage msg s@(HeirOfValandil a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      n <- selectCount (EnemyEngagedWith $ PlayerWithId pid)
      when (n > 0) $ phaseModifier a (PlayerTarget pid) (LowerNextCostOf (CardWithType AllyType <> CardWithTrait Dunedain) n)
      pure s
    _ -> HeirOfValandil <$> runMessage msg a
