module LordOfTheRings.Location.Cards.TwistingHollow (twistingHollow) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (mayFlipToGrottoSide)

-- | Underwater side of Cursed Caverns.
newtype TwistingHollow = TwistingHollow LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

twistingHollow :: LocationCard TwistingHollow
twistingHollow = location TwistingHollow Cards.twistingHollow

-- | While Twisting Hollow is the active location, characters cannot ready from card
-- effects; the refresh phase still readies them.
instance HasModifiersFor TwistingHollow where
  getModifiersFor (TwistingHollow a) = when (a.placement == ActiveLocation) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [CannotReadyFromCardEffects]

-- | Forced: If Twisting Hollow is the active location at the end of the quest phase, each
-- player must discard 1 character. Then, you may flip Twisting Hollow to its Grotto side.
instance HasAbilities TwistingHollow where
  getAbilities (TwistingHollow a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == ActiveLocation]

instance RunMessage TwistingHollow where
  runMessage msg l@(TwistingHollow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> chooseAndDiscard a pid =<< charactersControlledBy pid
      mayFlipToGrottoSide a
      pure l
    _ -> TwistingHollow <$> runMessage msg a
