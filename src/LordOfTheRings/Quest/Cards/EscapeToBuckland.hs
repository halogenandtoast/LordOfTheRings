module LordOfTheRings.Quest.Cards.EscapeToBuckland (escapeToBuckland) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (nazgulEngage, revealBlackRider)

newtype EscapeToBuckland = EscapeToBuckland QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

escapeToBuckland :: QuestCard EscapeToBuckland
escapeToBuckland = quest EscapeToBuckland Cards.escapeToBuckland

-- | Response: After a player makes a successful Hide test, place X progress on a non-unique
-- location in play. X is twice the number of players in the game.
instance HasAbilities EscapeToBuckland where
  getAbilities (EscapeToBuckland a) =
    [restricted a 1 (LocationExists $ NotLocation UniqueLocation) $ response $ PassedHideTestWindow After AnyPlayer | a.flipped]

{- | When Revealed (3A): Add Bucklebury Ferry to the staging area. Then 3B's When Revealed:
The first player searches the encounter deck and discard pile for a Black Rider, reveals
it (cannot be canceled), and adds it to the staging area. Shuffle the encounter deck.

When a player fails a Hide test, each Nazgûl enemy in the staging area engages that
player. (Trigger Forced effects after resolving this effect.)

The players win as Bucklebury Ferry is explored: this stage has no quest points.
-}
instance RunMessage EscapeToBuckland where
  runMessage msg q@(EscapeToBuckland attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      addSetAsideToStaging (cardIs Locations.buckleburyFerry)
      askToResolveWhenRevealed attrs
      EscapeToBuckland <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      revealBlackRider attrs
      pure q
    FailHideTest pid | attrs.flipped -> do
      nazgulEngage pid
      pure q
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      locations <- select (NotLocation UniqueLocation)
      chooseOrRunOneM pid do
        questionLabeled "escapeToBuckland.progress"
        causedBy attrs
        targets locations \l -> placeProgress attrs l (2 * players)
      pure q
    _ -> EscapeToBuckland <$> runMessage msg attrs
