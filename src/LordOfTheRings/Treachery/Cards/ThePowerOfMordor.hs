module LordOfTheRings.Treachery.Cards.ThePowerOfMordor (thePowerOfMordor) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Placement (Placement (StagingArea, Unplaced))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ThePowerOfMordor = ThePowerOfMordor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

thePowerOfMordor :: TreacheryCard ThePowerOfMordor
thePowerOfMordor = treachery ThePowerOfMordor Cards.thePowerOfMordor

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor ThePowerOfMordor where
  getModifiersFor (ThePowerOfMordor a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | Doomed 3.
-- When Revealed: Count the number of encounter cards in the staging area and shuffle them
-- into the encounter deck. Reveal an equal number of cards from the encounter deck and add
-- them to the staging area. This effect cannot be canceled. (Errata: "Reveal", once "Then,
-- reveal".)
-- Every encounter card there counts: enemies, locations, objectives and treacheries.
instance RunMessage ThePowerOfMordor where
  runMessage msg t@(ThePowerOfMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      enemies <- map toTarget <$> select EnemyInStagingArea
      locations <- map toTarget <$> select LocationInStagingArea
      others <- getsGame \g ->
        [toTarget o | o <- toList g.entities.objectives, o.placement == StagingArea]
          <> [toTarget x | x <- toList g.entities.treacheries, x.placement == StagingArea]
      let staged = enemies <> locations <> others
      traverse_ shuffleIntoEncounterDeck staged
      revealEncounterCards (length staged)
      pure t
    _ -> ThePowerOfMordor <$> runMessage msg attrs
