module LordOfTheRings.Ally.Cards.ImladrisCaregiver (imladrisCaregiver) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype ImladrisCaregiver = ImladrisCaregiver AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

imladrisCaregiver :: AllyCard ImladrisCaregiver
imladrisCaregiver = ally ImladrisCaregiver Cards.imladrisCaregiver

-- | Action: Discard a card from your hand to heal 1 damage on any character. (Limit twice
-- per round.)
instance HasAbilities ImladrisCaregiver where
  getAbilities (ImladrisCaregiver a) =
    [ limited (GroupLimit PerRound 2) $ mkAbility a 1 $ ActionAbility (DiscardFromHandCost pid 1)
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage ImladrisCaregiver where
  runMessage msg s@(ImladrisCaregiver a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOrRunOneM pid do
        questionLabeled "imladrisCaregiver.heal"
        causedBy a
        targets damaged \c -> healDamage a c 1
      pure s
    _ -> ImladrisCaregiver <$> runMessage msg a
