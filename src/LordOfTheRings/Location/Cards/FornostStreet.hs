module LordOfTheRings.Location.Cards.FornostStreet (fornostStreet) where

import LordOfTheRings.Card (CardType (EnemyType), cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (encounterDiscardPile)
import LordOfTheRings.Trait (Trait (Fornost, Undead))

newtype FornostStreet = FornostStreet LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fornostStreet :: LocationCard FornostStreet
fornostStreet = location FornostStreet Cards.fornostStreet

-- | While Fornost Street is in the staging area, each other Fornost location gets +1
-- threat.
instance HasModifiersFor FornostStreet where
  getModifiersFor (FornostStreet a) = when (a.placement == StagingArea) do
    others <- select (LocationWithTrait Fornost <> not_ (LocationWithId a.id))
    for_ others \l -> modified a l [ThreatModifier 1]

-- | Travel: Return the topmost enemy in the encounter discard pile to the staging area.
-- Shadow: Each Undead enemy engaged with you gets +1 attack and +1 defense until the end of
-- the phase.
instance RunMessage FornostStreet where
  runMessage msg l@(FornostStreet a) = case msg of
    TravelTo lid | lid == a.id -> do
      discards <- encounterDiscardPile
      traverse_ addToStagingFromDiscard $ find ((== EnemyType) . cdCardType . toCardDef) discards
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      undead <- select (EnemyWithTrait Undead <> EnemyEngagedWith (PlayerWithId pid))
      for_ undead \e -> phaseModifiers a e [AttackModifier 1, DefenseModifier 1]
      pure l
    _ -> FornostStreet <$> runMessage msg a
