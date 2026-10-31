module LordOfTheRings.Treachery.Cards.PursuitOnTheShore (pursuitOnTheShore) where

import LordOfTheRings.Card (Card (EncounterCard), CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Scenario.Types (scenarioVictoryDisplay)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PursuitOnTheShore = PursuitOnTheShore TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pursuitOnTheShore :: TreacheryCard PursuitOnTheShore
pursuitOnTheShore = treachery PursuitOnTheShore Cards.pursuitOnTheShore

-- | (Cannot be canceled.)
instance HasModifiersFor PursuitOnTheShore where
  getModifiersFor (PursuitOnTheShore a) = modifySelf a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Search the victory display and encounter discard pile for the enemy with
-- the most hit points. Return that enemy to the staging area. If no enemy is returned by
-- this effect, Pursuit on the Shore gains surge.
instance RunMessage PursuitOnTheShore where
  runMessage msg t@(PursuitOnTheShore attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      victory <- getsGame (scenarioVictoryDisplay . toAttrs . gameScenario)
      discardPile <- getEncounterDiscard
      let isEnemy c = cdCardType (toCardDef c) == EnemyType
          hp c = fromMaybe 0 (cdHitPoints $ toCardDef c)
          fromVictory = [(c, True) | EncounterCard c <- victory, isEnemy c]
          fromDiscard = [(c, False) | c <- discardPile, isEnemy c]
          found = fromVictory <> fromDiscard
          most = foldr (max . hp . fst) 0 found
      if null found
        then revealEncounterCard
        else do
          lead <- getLead
          chooseOrRunOneM lead do
            questionLabeled "pursuitOnTheShore.return"
            causedBy attrs
            for_ [x | x@(c, _) <- found, hp c == most] \(c, inVictory) ->
              targeting c
                $ if inVictory
                  then removeFromVictoryDisplay (EncounterCard c) >> addToStaging c
                  else addToStagingFromDiscard c
      pure t
    _ -> PursuitOnTheShore <$> runMessage msg attrs
