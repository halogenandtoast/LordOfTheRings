module LordOfTheRings.Ally.Cards.WildStallion (wildStallion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype WildStallion = WildStallion AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildStallion :: AllyCard WildStallion
wildStallion = ally WildStallion Cards.wildStallion

-- | Cannot have attachments.
instance HasModifiersFor WildStallion where
  getModifiersFor (WildStallion a) = modifySelf a [CannotHaveAttachments]

-- | The allies it may attach to: another you control that can take it, restricted, and
-- has no Wild Stallion already (Limit 1 per ally).
hosts :: AllyAttrs -> AllyMatcher
hosts a = AllyControlledBy (PlayerWithId a.owner) <> NotAlly (AllyWithId a.id) <> NotAlly (AllyWithAttachment $ toCardCode Cards.wildStallion)

-- | Planning Action: Attach Wild Stallion to an ally you control. (Counts as a Mount
-- attachment with the text: "Restricted. Limit 1 per ally. Attached ally gets +1
-- [willpower], +1 [attack], +1 [defense], and +1 hit point.") The Wild Stallion
-- attachment, built from this card, has that text.
instance HasAbilities WildStallion where
  getAbilities (WildStallion a) =
    [restricted a 1 (DuringPhase PlanningPhase <> exists (hosts a)) $ ActionAbility Free]

instance RunMessage WildStallion where
  runMessage msg s@(WildStallion a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      allies <- filterM canHost =<< select (hosts a)
      chooseOrRunOneM pid do
        questionLabeled "wildStallion.ally"
        causedBy a
        targets allies (attachAllyTo a)
      pure s
    _ -> WildStallion <$> runMessage msg a
   where
    canHost ally' = (\ms -> CannotHaveAttachments `notElem` ms && CannotHaveRestrictedAttachments `notElem` ms) <$> getModifiers ally'
