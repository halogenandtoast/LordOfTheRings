module LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers
  (cityStrength, damageAnnuminas, repairAnnuminas) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Message (Message (ContributeShared))
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Source (Sourceable)

cityStrength :: HasGame m => m Int
cityStrength = getsGame $ \g ->
  fromMaybe
    (sum [o.resources | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.annuminas])
    (scenarioEpic (toAttrs g.scenario) >>= annuminasCityStrength . epicShared)

damageAnnuminas, repairAnnuminas :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageAnnuminas source amount = changeCityStrength source (negate amount)
repairAnnuminas = changeCityStrength

changeCityStrength :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
changeCityStrength source amount = do
  shared <- getsGame $ scenarioEpic . toAttrs . (.scenario)
  if isJust (shared >>= annuminasCityStrength . epicShared)
    then push $ ContributeShared AnnuminasCityStrength amount
    else do
      cities <- getsGame $ \g -> [toAttrs o | o <- toList g.entities.objectives, toCardCode o == toCardCode Objectives.annuminas]
      for_ cities $ \oid ->
        if amount < 0 then dealDamage source oid (negate amount) else placeResources source oid amount
