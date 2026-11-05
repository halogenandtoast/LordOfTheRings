module LordOfTheRings.Ally.Cards.LoyalHound (loyalHound) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype LoyalHound = LoyalHound AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

loyalHound :: AllyCard LoyalHound
loyalHound = ally LoyalHound Cards.loyalHound

-- | Cannot have attachments.
instance HasModifiersFor LoyalHound where
  getModifiersFor (LoyalHound a) = modifySelf a [CannotHaveAttachments]

-- | Response: Discard Loyal Hound to cancel up to 2 damage just dealt to a hero you
-- control.
instance HasAbilities LoyalHound where
  getAbilities (LoyalHound a) =
    [ mkAbility a 1 $ response $ MatchingHeroDealtDamageWindow After (HeroControlledBy $ PlayerWithId pid)
    | Just pid <- [placementController a.placement]
    ]

-- | The damage is on the hero, its destruction not yet checked: the canceled points come
-- off before it is, as Honour Guard's.
instance RunMessage LoyalHound where
  runMessage msg s@(LoyalHound a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      discard a a
      mine <- select (HeroControlledBy $ PlayerWithId pid)
      for_ (take 1 [(t, n) | Window _ (DealtDamage t@(HeroTarget hid) n) <- windows, hid `elem` mine]) \(t, n) ->
        removeTokens a t Damage (min 2 n)
      pure s
    _ -> LoyalHound <$> runMessage msg a
