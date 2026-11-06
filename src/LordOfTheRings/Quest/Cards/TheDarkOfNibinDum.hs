module LordOfTheRings.Quest.Cards.TheDarkOfNibinDum (theDarkOfNibinDum) where

import LordOfTheRings.Card (CardType (LocationType), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (attachCaveTorchToFirstPlayer)

newtype TheDarkOfNibinDum = TheDarkOfNibinDum QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDarkOfNibinDum :: QuestCard TheDarkOfNibinDum
theDarkOfNibinDum = quest TheDarkOfNibinDum Cards.theDarkOfNibinDum

instance RunMessage TheDarkOfNibinDum where
  runMessage msg q@(TheDarkOfNibinDum attrs) = case msg of
    DoStep 7 Setup -> askToPerformSetup attrs >> pure q
    PerformSetup (isTarget attrs -> True) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ [Enemies.goblinChieftain, Locations.crackedPillar] \def ->
        traverse_ setAside $ find ((== toCardCode def) . toCardCode) deck
      attachCaveTorchToFirstPlayer attrs
      shuffleEncounterDeck
      turnOver attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheDarkOfNibinDum <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      searchDifferentEncounterCards players attrs (CardWithType LocationType) False
      pure q
    _ -> TheDarkOfNibinDum <$> runMessage msg attrs
