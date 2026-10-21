module LordOfTheRings.Ally.Cards.EreborRecordKeeper (ereborRecordKeeper) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Sphere (Sphere (Lore))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype EreborRecordKeeper = EreborRecordKeeper AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborRecordKeeper :: AllyCard EreborRecordKeeper
ereborRecordKeeper = ally EreborRecordKeeper Cards.ereborRecordKeeper

-- | Erebor Record Keeper cannot attack or defend.
instance HasModifiersFor EreborRecordKeeper where
  getModifiersFor (EreborRecordKeeper a) = modifySelf a [CannotAttackOrDefend]

-- | Action: Exhaust Erebor Record Keeper and pay 1 Lore resource to choose and ready a
-- Dwarf character.
instance HasAbilities EreborRecordKeeper where
  getAbilities (EreborRecordKeeper a) =
    [ restricted a 1 (AnyCriteria [exists (HeroWithTrait Dwarf <> ExhaustedHero), exists (AllyWithTrait Dwarf <> ExhaustedAlly)])
        $ ActionAbility (Costs [ExhaustCost $ toTarget a, SphereResourceCost pid Lore 1])
    | Just pid <- [placementController a.placement]
    ]

instance RunMessage EreborRecordKeeper where
  runMessage msg s@(EreborRecordKeeper a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      dwarves <- selectCharacters (HeroWithTrait Dwarf <> ExhaustedHero) (AllyWithTrait Dwarf <> ExhaustedAlly)
      chooseOneM pid do
        questionLabeled "ereborRecordKeeper.ready"
        causedBy a
        targets dwarves ready
      pure s
    _ -> EreborRecordKeeper <$> runMessage msg a
