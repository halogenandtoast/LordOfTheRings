-- | What Under the Ash Mountains' cards share: their X, 1 more than the number of quest
-- cards in play, and the dwindling supplies both stages' side B read.
module LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers (
  ashX,
  dwindlingSuppliesModifiers,
  dwindlingSupplies,
  runDwindlingSupplies,
) where

import LordOfTheRings.Ability
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (EventsCannotShuffleDiscardIntoDeck))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (QuestAttrs)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source (isSource)
import LordOfTheRings.Target (Target (GameTarget))

-- | "X is 1 more than the number of quest cards in play": the main quest and each side
-- quest.
ashX :: HasGame m => m Int
ashX = (+ 1) <$> questCardsInPlay

-- | "Event cards cannot shuffle cards from a player's discard pile into his deck."
dwindlingSuppliesModifiers :: HasModifiersM m => QuestAttrs -> m ()
dwindlingSuppliesModifiers a = when a.flipped $ modified a GameTarget [EventsCannotShuffleDiscardIntoDeck]

-- | "Forced: At the end of the round, each player discards X cards from the top of his
-- deck ..." The ability takes index 101.
dwindlingSupplies :: QuestAttrs -> [Ability]
dwindlingSupplies a = [mkAbility a 101 $ forced RoundEndsWindow | a.flipped]

{- | Each player discards X cards from the top of his deck, X counted as the effect
resolves. "If a player has no cards left in his deck, he is eliminated from the game" is
part of the same Forced effect, read once the cards are discarded: a deck run out at any
other time eliminates no one until the end of the round (rule sheet, Strategy Tip: "If you
have no cards in your deck at the end of the round, ... you're eliminated").
-}
runDwindlingSupplies :: Message -> QuestAttrs -> QueueT Message GameT ()
runDwindlingSupplies msg a = case msg of
  UseCardAbility _ (isSource a -> True) 101 _ -> do
    x <- ashX
    eachPlayer \p -> discardTopOfDeck p x
    doStep 1 msg
  DoStep 1 (UseCardAbility _ (isSource a -> True) 101 _) -> do
    players <- getRemainingPlayers
    starving <- filterM (fmap (null . (.deck)) . getPlayer) players
    traverse_ eliminatePlayer starving
  _ -> pure ()
