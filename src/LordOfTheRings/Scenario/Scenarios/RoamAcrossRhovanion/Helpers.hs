-- | What the stages of Roam Across Rhovanion share: Urdug, and the objectives his stages 2
-- set an encounter card to guard.
module LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion.Helpers (
  firstPlayerControls,
  guardOf,
  searchAndGuard,
) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (ObjectiveAllyType), cdCardType, toCardCode, toCardDef)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Objective.Types (objectiveGuard)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

-- | The first player controls the objective ally, free of encounters, as "if the first
-- player controls Urdug" reads.
firstPlayerControls :: HasGame m => CardDef -> m Bool
firstPlayerControls def = do
  lead <- getLead
  selectAny (AllyWithCardCode (toCardCode def) <> AllyControlledBy (PlayerWithId lead))

-- | The card id of the encounter card guarding the objective or objective ally, as "the Grey
-- Moorland guarding Tiny" reads.
guardOf :: HasGame m => CardDef -> m (Maybe CardId)
guardOf def = do
  allies <- getsGame (toList . (.entities.allies))
  objectives <- getsGame (toList . (.entities.objectives))
  let code = toCardCode def
  pure
    $ headMay
    $ [cid | a <- allies, toCardCode a == code, AttachedTo (CardIdTarget cid) <- [a.placement]]
    <> [cid | o <- objectives, toCardCode o == code, Just cid <- [objectiveGuard (toAttrs o)]]

{- | Each stage 2A's When Revealed: "The first player searches the encounter deck, discard
pile, and victory display for a (guard), adds it to the staging area, and attaches the
set-aside (objective) to it as a guarded objective. Shuffle the encounter deck. Each other
player reveals an encounter card."

With no such guard to find, as when it is already in play, nothing guards the objective:
it is free of encounters and goes where that puts it.
-}
searchAndGuard :: ReverseQueue m => CardDef -> CardDef -> m ()
searchAndGuard captor objective = do
  s <- getsGame (toAttrs . (.scenario))
  let aside = find (cardMatches $ cardIs objective) (scenarioSetAside s)
      findable =
        any (cardMatches $ cardIs captor) (s.encounterDeck <> s.encounterDiscard <> [c | EncounterCard c <- s.victoryDisplay])
      isAlly = (== ObjectiveAllyType) . cdCardType . toCardDef
  lead <- getLead
  if findable
    then searchEncounterCardsAndVictoryDisplay (cardIs captor) \c -> do
      addToStaging c
      for_ aside \o ->
        if isAlly o then guardCardWith (EncounterCard o) (EncounterCard c) else putObjectiveIntoPlayGuardedBy o c.id
    else do
      for_ aside \o -> if isAlly o then putObjectiveAllyIntoPlayUnder lead o else push (PutObjectiveIntoPlay o)
      shuffleEncounterDeck
  others <- drop 1 <$> getRemainingPlayers
  revealEncounterCards (length others)
