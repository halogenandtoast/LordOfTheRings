module LordOfTheRings.Quest.Cards.TheMountainsPeaks (theMountainsPeaks) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (cdVictoryPoints), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheMountainsPeaks = TheMountainsPeaks QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMountainsPeaks :: QuestCard TheMountainsPeaks
theMountainsPeaks = quest TheMountainsPeaks Cards.theMountainsPeaks

-- | Characters are discarded from play if their [Willpower] is ever 0.
instance HasModifiersFor TheMountainsPeaks where
  getModifiersFor (TheMountainsPeaks a) = do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [DiscardedAtNoWillpower]

-- | When Revealed: Shuffle all copies of Snowstorm in the encounter discard pile back into
-- the encounter deck.
-- Players cannot defeat this stage unless they have 5 victory points. If the players
-- defeat this stage, they have won the game.
instance RunMessage TheMountainsPeaks where
  runMessage msg q@(TheMountainsPeaks attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheMountainsPeaks <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck (cardIs Treacheries.snowstorm)
      pure q
    _ -> TheMountainsPeaks <$> runConditionalStage condition msg attrs
   where
    -- Nightmare: "player cards in the victory display do not count their victory points"
    condition = do
      nightmare <- inNightmareMode
      victory <- getsGame ((.victoryDisplay) . toAttrs . (.scenario))
      let counted = if nightmare then [c | c@(EncounterCard _) <- victory] else victory
      pure $ sum (mapMaybe (cdVictoryPoints . toCardDef) counted) >= 5
