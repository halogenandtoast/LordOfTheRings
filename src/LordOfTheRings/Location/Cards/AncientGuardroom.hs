module LordOfTheRings.Location.Cards.AncientGuardroom (ancientGuardroom) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype AncientGuardroom = AncientGuardroom LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientGuardroom :: LocationCard AncientGuardroom
ancientGuardroom = location AncientGuardroom Cards.ancientGuardroom

-- | While Ancient Guardroom is the active location, each enemy in play gets -1 attack.
-- With no encounter card left to discard, its Travel cost cannot be paid.
instance HasModifiersFor AncientGuardroom where
  getModifiersFor (AncientGuardroom a) = do
    when (a.placement == ActiveLocation) do
      enemies <- select AnyEnemy
      for_ enemies \e -> modified a e [AttackModifier (-1)]
    scenario <- getsGame (toAttrs . (.scenario))
    when (null scenario.encounterDeck && null scenario.encounterDiscard) $ modifySelf a [CannotBeTraveledTo]

-- | Travel: Discard the top card of the encounter deck to travel here. Each player raises
-- his threat by X, where X is the discarded card's threat.
instance RunMessage AncientGuardroom where
  runMessage msg l@(AncientGuardroom a) = case msg of
    TravelTo lid | lid == a.id -> do
      discardTopOfEncounterDeck a 1
      AncientGuardroom <$> runMessage msg a
    DiscardedTopOfEncounterDeck (isSource a -> True) _ cards -> do
      let x = sum $ mapMaybe (cdThreat . toCardDef) cards
      when (x > 0) $ eachPlayer \p -> encounterRaisesThreat p x
      pure l
    _ -> AncientGuardroom <$> runMessage msg a
