module LordOfTheRings.Objective.Cards.CaveTorch (caveTorch) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Dark))

newtype CaveTorch = CaveTorch ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveTorch :: ObjectiveCard CaveTorch
caveTorch = objective CaveTorch Cards.caveTorch

-- | Attach to a hero. Restricted.
-- Action: Exhaust Cave Torch to place up to 3 progress on a Dark location.
-- Forced: After Cave Torch exhausts, discard the top card of the encounter deck. If that
-- card is an enemy, add it to the staging area.
instance HasAbilities CaveTorch where
  getAbilities (CaveTorch a) = case a.placement of
    AttachedTo _ ->
      [ restricted a 1 (exists $ LocationWithTrait Dark) $ ActionAbility (exhaustCost a)
      , mkAbility a 2 $ forced $ ExhaustedWindow After (toTarget a)
      ]
    _ -> []

instance RunMessage CaveTorch where
  runMessage msg o@(CaveTorch attrs) = case msg of
    -- ponytail: "up to 3" always places 3; fewer is never worse to skip
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      dark <- select (LocationWithTrait Dark)
      chooseOrRunOneM pid do
        questionLabeled "caveTorch.location"
        causedBy attrs
        targets dark \l -> placeProgress attrs l 3
      pure o
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      discardTopOfEncounterDeck attrs 1
      pure o
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter ((== EnemyType) . cdCardType . toCardDef) cards) \c -> do
        removeFromEncounterDiscard c
        addToStaging c
      pure o
    _ -> CaveTorch <$> runMessage msg attrs
