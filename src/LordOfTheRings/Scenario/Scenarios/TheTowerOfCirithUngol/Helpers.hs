-- | What The Tower of Cirith Ungol's cards share: the Orcs fighting among themselves, as
-- Torch-lit Hall and stage 2B discard for an enemy, and the stage the players are at.
module LordOfTheRings.Scenario.Scenarios.TheTowerOfCirithUngol.Helpers (
  orcsFight,
  resolveOrcsFight,
  atStage,
) where

import LordOfTheRings.Card (CardType (EnemyType), EncounterCard, cdName, toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Card.CardDef (cdStage)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target (Targetable)

{- | "Discard cards from the top of the encounter deck until an enemy is discarded": what
was discarded comes back to the card as 'DiscardedTopOfEncounterDeck' with the target, for
'resolveOrcsFight'.
-}
orcsFight :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
orcsFight source target = discardEncounterCardsUntilFor source target (CardWithType EnemyType)

{- | "If there is a copy of that enemy in the staging area, discard that copy as well. If
there is not, add the discarded enemy to the staging area." A copy is an enemy of its
title; with several, the first player chooses which.
-}
resolveOrcsFight :: ReverseQueue m => [EncounterCard] -> m ()
resolveOrcsFight cards = for_ (find (cardMatches $ CardWithType EnemyType) cards) \enemy -> do
  let Name title _ = cdName (toCardDef enemy)
  copies <- select (EnemyInStagingArea <> EnemyWithTitle title)
  if null copies
    then addToStagingFromDiscard enemy
    else do
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "orcsFight.discardCopy"
        targets copies discard

-- | The main quest is at that stage, on its B side, with at least that much progress, as
-- Topmost Chamber reads "at least 18 progress on stage 2B".
atStage :: HasGame m => Int -> Int -> m Bool
atStage n progress =
  getsGame
    $ any (\q -> let a = toAttrs q in cdStage (toCardDef a) == Just n && a.flipped && a.progress >= progress)
    . toList
    . entitiesQuests
    . gameEntities
