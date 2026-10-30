module LordOfTheRings.Quest.Cards.TheShadesOfAngmar (theShadesOfAngmar) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheShadesOfAngmar = TheShadesOfAngmar QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

theShadesOfAngmar :: QuestCard TheShadesOfAngmar
theShadesOfAngmar = quest TheShadesOfAngmar Cards.theShadesOfAngmar

-- | If a player has no cards in his deck, he is eliminated from the game.
instance HasModifiersFor TheShadesOfAngmar where
  getModifiersFor (TheShadesOfAngmar a) = when a.flipped $ modified a GameTarget [EliminatedWithEmptyDeck]

-- | Forced: At the end of the refresh phase, each player discards the top card of his deck.
instance HasAbilities TheShadesOfAngmar where
  getAbilities (TheShadesOfAngmar a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | a.flipped]

-- | Setup: Put Iârion into play. Set Thaurdir aside, out of play. Add 1 copy of Fornost
-- Square to the staging area. Shuffle the encounter deck.
-- When Revealed: Each player must search the encounter deck for a Baleful Shade and put it
-- into play engaged with him. Shuffle the encounter deck and reveal 1 encounter card per
-- player.
-- The first player gains control of Iârion as he comes out.
instance RunMessage TheShadesOfAngmar where
  runMessage msg q@(TheShadesOfAngmar attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      let firstOf m = for_ (take 1 $ filter (cardMatches m) deck) \card -> do
            removeFromEncounterDeck card
            addToStaging card
      firstOf (cardIs Objectives.iarion)
      traverse_ setAside $ filter (cardMatches $ cardIs Enemies.thaurdir) deck
      firstOf (cardIs Locations.fornostSquare)
      shuffleEncounterDeck
      askToResolveWhenRevealed attrs
      pure $ TheShadesOfAngmar $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (zip players $ filter (cardMatches $ cardIs Enemies.balefulShade) deck) \(pid, card) -> do
        removeFromEncounterDeck card
        putEnemyIntoPlayEngaged pid card
      shuffleEncounterDeck
      revealEncounterCards (length players)
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \p -> discardTopOfDeck p 1
      pure q
    _ -> TheShadesOfAngmar <$> runMessage msg attrs
