module LordOfTheRings.Quest.Cards.EmbassyFromTheDarkTower (embassyFromTheDarkTower) where

import Data.List (transpose)
import LordOfTheRings.Card (Card (EncounterCard, PlayerCard), EncounterCard, PlayerCard, cdThreat, toCardCode, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Player.Runner (canPlayAtNoCost)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype EmbassyFromTheDarkTower = EmbassyFromTheDarkTower QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

embassyFromTheDarkTower :: QuestCard EmbassyFromTheDarkTower
embassyFromTheDarkTower = quest EmbassyFromTheDarkTower Cards.embassyFromTheDarkTower

-- | A faceup player card, with its owner, and the encounter card paired with it.
type Pair = ((PlayerId, PlayerCard), EncounterCard)

{- | Setup: Add The Black Gate and The Mouth of Sauron to the staging area. Set Gwaihir
aside, out of play (the scenario sets the three aside as it gathers the encounter deck).
Shuffle the encounter deck.

Dire. When Revealed: In player order, each player places the top card of his deck faceup in
front of him until there are a total of 4 faceup player cards between the players. Place
the top 4 cards of the encounter deck faceup. Starting with the highest cost player card,
pair each player card with the highest threat encounter card remaining until there are 4
pairs. For each pair, the first player must choose: either discard both cards in that pair,
or reveal the encounter card to play the player card at no cost.

Cards tied for the highest cost, or the highest threat, are of the first player's choosing
where they differ. A player card that cannot be played once its encounter card is revealed
is discarded. With no quest points, the stage is done once the pairs are: the players
advance to stage 2.
-}
instance RunMessage EmbassyFromTheDarkTower where
  runMessage msg q@(EmbassyFromTheDarkTower attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.theBlackGate)
      addSetAsideToStaging (cardIs Enemies.theMouthOfSauron)
      shuffleEncounterDeck
      EmbassyFromTheDarkTower <$> runMessage msg attrs
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      EmbassyFromTheDarkTower <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      decks <- for players \p -> map (p,) . (\pl -> [pc | PlayerCard pc <- pl.deck]) <$> getPlayer p
      -- one card from each player in turn, round after round, until there are 4
      let faceup = take 4 $ concat $ transpose decks
      for_ faceup \(p, pc) -> removeFromDeck p pc.id
      encounter <- take 4 <$> getsGame ((.encounterDeck) . toAttrs . (.scenario))
      traverse_ removeFromEncounterDeck encounter
      pairUp [] faceup encounter
      pure q
    DoStep 1 (PlayAtNoCost owner pc) -> do
      playable <- canPlayAtNoCost owner (PlayerCard pc)
      if playable then playAtNoCost owner pc else putInDiscard pc
      pure q
    _ -> EmbassyFromTheDarkTower <$> runMessage msg attrs
   where
    cost = printedCostOf . snd
    threat = fromMaybe 0 . cdThreat . toCardDef
    -- the highest cost player card left takes the highest threat encounter card left
    pairUp :: ReverseQueue m => [Pair] -> [(PlayerId, PlayerCard)] -> [EncounterCard] -> m ()
    pairUp made players@(_ : _) encounter@(_ : _) = do
      lead <- getLead
      let top = foldr (max . cost) 0 players
          costliest = [p | p <- players, cost p == top]
      -- the first player picks among tied cards that differ; alike, any will do
      if length (ordNub $ map (toCardCode . snd) costliest) <= 1
        then for_ (headMay costliest) (pairWith made players encounter)
        else chooseOneM lead do
          questionLabeled "embassyFromTheDarkTower.tied"
          causedBy attrs
          showing (map (PlayerCard . snd) costliest)
          for_ costliest \p -> targeting (CardIdTarget (snd p).id) (pairWith made players encounter p)
    pairUp made players encounter = do
      -- a card left without a partner, as with fewer than 4 of one kind, is discarded
      for_ players (putInDiscard . snd)
      unless (null encounter) $ push $ AddToEncounterDiscard encounter
      resolvePairs made
    pairWith :: ReverseQueue m => [Pair] -> [(PlayerId, PlayerCard)] -> [EncounterCard] -> (PlayerId, PlayerCard) -> m ()
    pairWith made players encounter player = do
      lead <- getLead
      let highest = foldr (max . threat) 0 encounter
          fiercest = [e | e <- encounter, threat e == highest]
          paired :: ReverseQueue n => EncounterCard -> n ()
          paired ec = pairUp (made <> [(player, ec)]) (filter ((/= (snd player).id) . (.id) . snd) players) (filter ((/= ec.id) . (.id)) encounter)
      if length (ordNub $ map toCardCode fiercest) <= 1
        then for_ (headMay fiercest) paired
        else chooseOneM lead do
          questionLabeled "embassyFromTheDarkTower.tied"
          causedBy attrs
          showing (map EncounterCard fiercest)
          for_ fiercest \ec -> targeting (CardIdTarget ec.id) (paired ec)
    resolvePairs :: ReverseQueue m => [Pair] -> m ()
    resolvePairs = \case
      [] -> advanceQuest
      (((owner, pc), ec) : rest) -> do
        lead <- getLead
        chooseOneM lead do
          questionLabeled "embassyFromTheDarkTower.pair"
          causedBy attrs
          showing [PlayerCard pc, EncounterCard ec]
          labeled "embassyFromTheDarkTower.discard" do
            putInDiscard pc
            push $ AddToEncounterDiscard [ec]
            resolvePairs rest
          labeled "embassyFromTheDarkTower.reveal" do
            push $ RevealCard ec
            -- whether it can be played is read once the encounter card has resolved
            doStep 1 (PlayAtNoCost owner pc)
            resolvePairs rest
