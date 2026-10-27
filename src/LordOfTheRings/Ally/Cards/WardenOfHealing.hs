module LordOfTheRings.Ally.Cards.WardenOfHealing (wardenOfHealing) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype WardenOfHealing = WardenOfHealing AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wardenOfHealing :: AllyCard WardenOfHealing
wardenOfHealing = ally WardenOfHealing Cards.wardenOfHealing

-- | Action: Exhaust Warden of Healing to heal 1 damage on up to 2 different characters.
-- Then, you may pay 2 lore resources to ready Warden of Healing.
instance HasAbilities WardenOfHealing where
  getAbilities (WardenOfHealing a) = [mkAbility a 1 $ ActionAbility (exhaustCost a)]

instance RunMessage WardenOfHealing where
  runMessage msg s@(WardenOfHealing a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "wardenOfHealing.heal"
        causedBy a
        targets damaged \c -> do
          healDamage a c 1
          chooseOneM pid do
            questionLabeled "wardenOfHealing.heal"
            causedBy a
            targets (filter (/= c) damaged) \c' -> healDamage a c' 1
            labeled "done" (pure ())
        labeled "done" (pure ())
      lore <- getResourcesOfSphere pid Lore
      when (lore >= 2) $ chooseOneM pid do
        questionLabeled "wardenOfHealing.ready"
        causedBy a
        labeled "wardenOfHealing.payToReady" do
          spendResources pid Lore 2
          ready a
        labeled "wardenOfHealing.doNotReady" (pure ())
      pure s
    _ -> WardenOfHealing <$> runMessage msg a
