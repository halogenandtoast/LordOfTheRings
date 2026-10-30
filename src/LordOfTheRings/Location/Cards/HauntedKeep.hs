module LordOfTheRings.Location.Cards.HauntedKeep (hauntedKeep) where

import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (discardTopCards, printedCost)
import LordOfTheRings.Trait (Trait (Undead))

newtype HauntedKeep = HauntedKeep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedKeep :: LocationCard HauntedKeep
hauntedKeep = location HauntedKeep Cards.hauntedKeep

-- | While Haunted Keep is the active location, the first Undead enemy revealed each round
-- gains surge. Read off the window after it is revealed, before its surge resolves, once a
-- round.
instance HasAbilities HauntedKeep where
  getAbilities (HauntedKeep a) =
    [ limited (GroupLimit PerRound 1) $ mkAbility a 1 $ silent $ EnemyWithTraitRevealedWindow After Undead
    | a.placement == ActiveLocation
    ]

-- | Shadow: Defending player discards the top card of his deck. Attacking enemy gets +X
-- attack where X is the discarded card's printed cost.
instance RunMessage HauntedKeep where
  runMessage msg l@(HauntedKeep a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- windows] \eid -> roundModifier a eid (GainKeyword Surge)
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      x <- sum . map printedCost <$> discardTopCards pid 1
      when (x > 0) $ enemyGetsAttack a eid x
      pure l
    _ -> HauntedKeep <$> runMessage msg a
