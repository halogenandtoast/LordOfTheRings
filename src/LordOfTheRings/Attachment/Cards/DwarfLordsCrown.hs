module LordOfTheRings.Attachment.Cards.DwarfLordsCrown (dwarfLordsCrown) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)

newtype DwarfLordsCrown = DwarfLordsCrown AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a hero.
dwarfLordsCrown :: AttachmentCard DwarfLordsCrown
dwarfLordsCrown = attachment DwarfLordsCrown Cards.dwarfLordsCrown

-- | Action: Exhaust Dwarf-Lord's Crown to give each ally that shares a Trait with attached
-- hero +1 [willpower], +1 [attack], and +1 [defense] until the end of the phase.
instance HasAbilities DwarfLordsCrown where
  getAbilities (DwarfLordsCrown a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | AttachedTo (HeroTarget _) <- [a.placement]]

instance RunMessage DwarfLordsCrown where
  runMessage msg s@(DwarfLordsCrown a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        traits <- toList <$> (getTraits (HeroTarget hid) . toCardDef =<< getHero hid)
        allies <- if null traits then pure [] else select (AllyMatchAny $ map AllyWithTrait traits)
        for_ allies \ally -> phaseModifiers a ally [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure s
    _ -> DwarfLordsCrown <$> runMessage msg a
