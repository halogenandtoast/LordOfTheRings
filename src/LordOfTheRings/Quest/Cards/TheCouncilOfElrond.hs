module LordOfTheRings.Quest.Cards.TheCouncilOfElrond (theCouncilOfElrond) where

import Data.List (transpose)
import LordOfTheRings.Card (Card (PlayerCard), PlayerCard)
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Player.Runner (canPlayAtNoCost)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheCouncilOfElrond = TheCouncilOfElrond QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCouncilOfElrond :: QuestCard TheCouncilOfElrond
theCouncilOfElrond = quest TheCouncilOfElrond Cards.theCouncilOfElrond

{- | Forced: At the end of the planning phase, each player places the top card of his deck
faceup in front of him, in player order, until there are a total of 4 faceup cards between
the players. The first player chooses 1 faceup card to play for 0 cost, 1 to add to its
owner's hand, 1 to discard, and 1 to shuffle into its owner's deck. Then, either shuffle
Lust for the Ring into the encounter deck, or raise each player's threat by 5. Advance to
stage 2.
-}
instance HasAbilities TheCouncilOfElrond where
  getAbilities (TheCouncilOfElrond a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase | a.flipped]

-- | What becomes of a faceup card, in the order the first player assigns them.
data Fate = Play | ToHand | ToDiscard | ToDeck
  deriving stock (Eq, Enum, Bounded)

{- | Setup: Set Lust for the Ring, Redhorn Pass, Doors of Durin and Watcher in the Water
aside, out of play (the scenario does, as it gathers the encounter deck). Shuffle the
encounter deck.
-}
instance RunMessage TheCouncilOfElrond where
  runMessage msg q@(TheCouncilOfElrond attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      shuffleEncounterDeck
      TheCouncilOfElrond <$> runMessage msg attrs
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      decks <- for players \p -> map (p,) . (\pl -> [pc | PlayerCard pc <- pl.deck]) <$> getPlayer p
      -- one card from each player in turn, round after round, until there are 4
      let faceup = take 4 $ concat $ transpose decks
      for_ faceup \(p, pc) -> removeFromDeck p pc.id
      assign [minBound ..] faceup
      pure q
    _ -> TheCouncilOfElrond <$> runMessage msg attrs
   where
    assign :: ReverseQueue m => [Fate] -> [(PlayerId, PlayerCard)] -> m ()
    assign (fate : fates) cards@(_ : _) = do
      lead <- getLead
      playable <- filterM (\(pid, pc) -> canPlayAtNoCost pid (PlayerCard pc)) cards
      -- a card that cannot be played is still the one chosen to be: it is discarded
      let eligible = if fate == Play && notNull playable then playable else cards
      chooseOneM lead do
        questionLabeled $ "theCouncilOfElrond." <> fateKey fate
        causedBy attrs
        showing (map (PlayerCard . snd) cards)
        for_ eligible \(owner, pc) -> targeting (CardIdTarget pc.id) do
          case fate of
            Play | (owner, pc) `elem` playable -> playAtNoCost owner pc
            Play -> putInDiscard pc
            ToHand -> putInHand pc
            ToDiscard -> putInDiscard pc
            ToDeck -> shuffleIntoDeck owner pc
          assign fates (filter ((/= pc.id) . (.id) . snd) cards)
    assign _ _ = do
      lead <- getLead
      lust <- getSetAside (cardIs Treacheries.lustForTheRing)
      chooseOrRunOneM lead do
        questionLabeled "theCouncilOfElrond.choose"
        causedBy attrs
        unless (null lust) $ labeled "theCouncilOfElrond.shuffleLust" $ shuffleSetAsideIntoEncounterDeck lust
        labeled "theCouncilOfElrond.raiseThreat" $ eachPlayer \p -> encounterRaisesThreat p 5
      advanceQuest
    fateKey = \case
      Play -> "play"
      ToHand -> "hand"
      ToDiscard -> "discard"
      ToDeck -> "shuffle"
