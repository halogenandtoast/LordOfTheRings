module LordOfTheRings.Quest.Cards.TheBridgeOfKhazadDum (theBridgeOfKhazadDum) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.JourneyInTheDark.Helpers (removeDoomDamage)

newtype TheBridgeOfKhazadDum = TheBridgeOfKhazadDum QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBridgeOfKhazadDum :: QuestCard TheBridgeOfKhazadDum
theBridgeOfKhazadDum = quest TheBridgeOfKhazadDum Cards.theBridgeOfKhazadDum

-- | No more than 5 progress can be placed on this stage each round. The players cannot
-- defeat this stage while The Great Bridge is in play.
instance HasModifiersFor TheBridgeOfKhazadDum where
  getModifiersFor (TheBridgeOfKhazadDum a) = when a.flipped do
    modifySelf a [ProgressPerRoundLimit 5]
    bridge <- selectAny (LocationWithCardCode $ toCardCode Locations.theGreatBridge)
    when bridge $ modifySelf a [CannotBeDefeated]

{- | When Revealed: Add The Great Bridge to the staging area. Each player reveals 1
encounter card and removes 1 damage token from Doom, Doom, Doom, in turn from the first
player: the "you" of the card he reveals.

If the players defeat this stage, they win the game: it is the last.
-}
instance RunMessage TheBridgeOfKhazadDum where
  runMessage msg q@(TheBridgeOfKhazadDum attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheBridgeOfKhazadDum <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theGreatBridge)
      eachPlayer \p -> do
        push $ SetRevealer (Just p)
        revealEncounterCard
        push $ SetRevealer Nothing
        removeDoomDamage attrs
      pure q
    _ -> TheBridgeOfKhazadDum <$> runMessage msg attrs
