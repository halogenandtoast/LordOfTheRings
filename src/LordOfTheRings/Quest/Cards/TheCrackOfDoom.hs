module LordOfTheRings.Quest.Cards.TheCrackOfDoom (theCrackOfDoom) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheCrackOfDoom = TheCrackOfDoom QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCrackOfDoom :: QuestCard TheCrackOfDoom
theCrackOfDoom = quest TheCrackOfDoom Cards.theCrackOfDoom

-- | Dire. Allies cannot enter play and 'when revealed' effects cannot be canceled.
instance HasModifiersFor TheCrackOfDoom where
  getModifiersFor (TheCrackOfDoom a) = when a.flipped $ modified a GameTarget [AlliesCannotEnterPlay, WhenRevealedCannotBeCanceled]

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
cards from the top until X enemies are discarded, where X is the number of players. Add
each enemy discarded this way to the staging area.

Forced: When Mount Doom is explored, the first player makes a Fortitude test. Discard 1
extra encounter card for each player. If the first player fails this test, the
Ring-bearer claims The One Ring for himself and the players lose the game. If the test is
successful, the Ring-bearer completes his quest and the players win the game. Read off the
window as Mount Doom is explored, while it is still in play.
-}
instance RunMessage TheCrackOfDoom where
  runMessage msg q@(TheCrackOfDoom attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheCrackOfDoom <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    -- the deck is read once it is shuffled
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    CheckWindows ws | attrs.flipped -> do
      explored <- filterM (\lid -> selectAny (LocationWithId lid <> LocationWithCardCode (toCardCode Locations.mountDoom))) [lid | Window When (Explored lid) <- ws]
      unless (null explored) do
        lead <- getLead
        players <- length <$> getRemainingPlayers
        fortitudeTestWith lead attrs (\t -> t {fortitudeExtraCards = players}) (gameOver Lost) winGame
      TheCrackOfDoom <$> runMessage msg attrs
    _ -> TheCrackOfDoom <$> runMessage msg attrs
