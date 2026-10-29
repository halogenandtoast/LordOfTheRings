module LordOfTheRings.Quest.Cards.Snowdrifts (snowdrifts) where

import LordOfTheRings.Card (Card (EncounterCard), toCardCode)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype Snowdrifts = Snowdrifts QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowdrifts :: QuestCard Snowdrifts
snowdrifts = quest Snowdrifts Cards.snowdrifts

-- | When Revealed: Shuffle 1 more copy of Snowstorm into the encounter deck than the
-- number of players in the game.
-- Forced: After placing the 11th progress token on Snowdrifts, discard any active
-- location. Caradhras becomes the active location.
instance RunMessage Snowdrifts where
  runMessage msg q@(Snowdrifts attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      Snowdrifts <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      setAside' <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      shuffleSetAsideIntoEncounterDeck $ take (players + 1) $ filter (cardMatches $ cardIs Treacheries.snowstorm) setAside'
      pure q
    -- before the stage is defeated by its last progress
    PlaceTokens _ (isTarget attrs -> True) Progress n
      | attrs.questPoints > 0 && attrs.progress < attrs.questPoints && attrs.progress + n >= attrs.questPoints -> do
          caradhras <- select (LocationWithCardCode $ toCardCode Locations.caradhras)
          active <- select IsActiveLocation
          for_ (filter (`notElem` caradhras) active) discard
          for_ caradhras makeActive
          -- explored already, it comes back from the victory display (FAQ 2.31)
          when (null caradhras) do
            victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
            for_ (take 1 [c | EncounterCard c <- victory, toCardCode c == toCardCode Locations.caradhras]) \c -> do
              removeFromVictoryDisplay (EncounterCard c)
              putIntoPlayAsActiveLocation c
          Snowdrifts <$> runMessage msg attrs
    _ -> Snowdrifts <$> runMessage msg attrs
