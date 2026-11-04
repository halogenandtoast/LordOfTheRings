module LordOfTheRings.Quest.Cards.AShortcutToMushrooms (aShortcutToMushrooms) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (nazgulEngage, revealBlackRider)

newtype AShortcutToMushrooms = AShortcutToMushrooms QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aShortcutToMushrooms :: QuestCard AShortcutToMushrooms
aShortcutToMushrooms = quest AShortcutToMushrooms Cards.aShortcutToMushrooms

-- | When a player makes a Hide test, add 1 to the total threat for each ally that player
-- controls.
instance HasModifiersFor AShortcutToMushrooms where
  getModifiersFor (AShortcutToMushrooms a) = when a.flipped do
    players <- getRemainingPlayers
    for_ players \p -> do
      allies <- length <$> select (AllyControlledBy $ PlayerWithId p)
      when (allies > 0) $ modified a p [HideTestThreat allies]

-- | When Revealed: The first player searches the encounter deck and discard pile for a
-- Black Rider, reveals it (cannot be canceled), and adds it to the staging area. Shuffle the
-- encounter deck.
--
-- When a player fails a Hide test, each Nazgûl enemy in the staging area engages that
-- player. (Trigger Forced effects after resolving this effect.)
instance RunMessage AShortcutToMushrooms where
  runMessage msg q@(AShortcutToMushrooms attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AShortcutToMushrooms <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      revealBlackRider attrs
      pure q
    FailHideTest pid | attrs.flipped -> do
      nazgulEngage pid
      pure q
    _ -> AShortcutToMushrooms <$> runMessage msg attrs
