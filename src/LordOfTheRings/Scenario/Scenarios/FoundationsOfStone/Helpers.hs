{- | What the cards of Foundations of Stone share: the Nameless enemies' facedown cards, and
the stage 4s the players draw at Washed Away!.

Stage 4 gives each player a staging area and a stage of his own, and the engine has one of
each. The players share them instead: each player draws a random stage 4 and its When
Revealed effect resolves for him at once, into the one staging area, then the group
quests through the drawn stages in turn, the first player's first, before stage 5. Joining
another player is moving on to the next drawn stage. Solo this is the card as printed.
-}
module LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers (
  attachFromTopOfDeck,
  namelessX,
  stageFours,
  stageFourRevealed,
) where

import LordOfTheRings.Card (Card (PlayerCard), CardDef (..), costAsResources, toCardDef)
import LordOfTheRings.Enemy.Types (EnemyAttrs)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

-- | The top cards of the player's deck go facedown under the enemy.
attachFromTopOfDeck :: (ReverseQueue m, Targetable target) => PlayerId -> Int -> target -> m ()
attachFromTopOfDeck pid n target = do
  deck <- (.deck) <$> getPlayer pid
  for_ [pc | PlayerCard pc <- take n deck] \pc -> do
    removeFromDeck pid pc.id
    attachFacedown target pc

{- | X is the printed cost of all attached cards on this card. If there are no cards
attached, X is the given number.
-}
namelessX :: Int -> EnemyAttrs -> Int
namelessX none attrs
  | null attrs.facedown = none
  | otherwise = sum [maybe 0 costAsResources (cdCost $ toCardDef pc) | pc <- attrs.facedown]

stageFours :: [CardDef]
stageFours = [Quests.belowTheMines, Quests.theEndlessCaves, Quests.theShiveringBank, Quests.oldOneLair]

{- | The When Revealed effect of the stage 4 the player drew, resolved for him. "Create your
own staging area" is the shared one.
-}
stageFourRevealed :: (ReverseQueue m, Sourceable source) => source -> PlayerId -> CardDef -> m ()
stageFourRevealed source pid stage
  -- Sheltered Rocks: Reveal 2 cards from the encounter deck and add them to your staging area.
  | is Quests.belowTheMines = revealEncounterCards 2
  -- The Endless Caves: Discard all resources from your heroes.
  | is Quests.theEndlessCaves = do
      heroes <- select (HeroControlledBy $ PlayerWithId pid)
      for_ heroes \h -> do
        n <- (.resources) <$> getHero h
        when (n > 0) $ removeTokens source h Resource n
  -- The Shivering Bank: Discard your hand. Reveal 2 cards from the encounter deck and add
  -- them to your staging area.
  | is Quests.theShiveringBank = discardHand pid >> revealEncounterCards 2
  -- Old One Lair: Reveal 4 cards from the encounter deck and add them to your staging area.
  | is Quests.oldOneLair = revealEncounterCards 4
  | otherwise = pure ()
 where
  is def = cdCardCode def == cdCardCode stage
