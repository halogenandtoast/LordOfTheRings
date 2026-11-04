module LordOfTheRings.Ally.Cards.MasterOfLore (masterOfLore) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (CardType (AllyType, AttachmentType, EventType))
import LordOfTheRings.Sphere (Sphere (Lore))

newtype MasterOfLore = MasterOfLore AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

masterOfLore :: AllyCard MasterOfLore
masterOfLore = ally MasterOfLore Cards.masterOfLore

-- | Action: Exhaust Master of Lore to name a card type. Lower the cost for you to play the
-- next lore card of that type by 1 until the end of the phase (to a minimum of 1). (FAQ
-- 1.9 errata; as printed, every lore card of that type.)
instance HasAbilities MasterOfLore where
  getAbilities (MasterOfLore a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage MasterOfLore where
  runMessage msg s@(MasterOfLore a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      errata <- playsWithErrata a.owner
      let lower t = phaseModifier a (PlayerTarget pid) $ (if errata then ReduceNextCostOf else ReduceCostOf) (CardWithType t <> CardWithSphere Lore) 1
      chooseOneM pid do
        questionLabeled "masterOfLore.name"
        causedBy a
        labeled "masterOfLore.ally" $ lower AllyType
        labeled "masterOfLore.attachment" $ lower AttachmentType
        labeled "masterOfLore.event" $ lower EventType
      pure s
    _ -> MasterOfLore <$> runMessage msg a
