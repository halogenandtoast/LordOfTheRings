{- | What the cards of Foundations of Stone share: the Nameless enemies' facedown cards, and
the stage 4s the players draw at Washed Away!.

At stage 4 each player creates his own staging area, as the rules insert says, and
quests through the stage 4 he drew there: see 'LordOfTheRings.Game.Base.Separation'. Each
area has its own staging area, active location and stage, and resolves each phase in
turn, as if its players alone were in the game; the uniqueness rule still holds across
the areas (FAQ 2.36), so Elladan and Elrohir lose their bonuses while apart (ALeP 3.040),
and each player is the first player of his own area while the token still passes
(ALeP 3.030). A player whose stage is defeated joins another at the beginning of the
travel phase, taking the encounter cards in his staging area and those engaged with him,
his active location discarded; once no one is left to join, the players continue on to
stage 5 together.
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

{- | The When Revealed effect of the stage 4 the player drew, resolved for him in his own
area, which Washed Away! has created.
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
