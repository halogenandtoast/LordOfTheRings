module LordOfTheRings.Location.Cards.TheMarish (theMarish) where

import LordOfTheRings.Card (Card (EncounterCard), CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisRound)
import LordOfTheRings.Trait (Trait (Nazgul))

newtype TheMarish = TheMarish LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMarish :: LocationCard TheMarish
theMarish = location TheMarish Cards.theMarish

-- | Forced: After a player fails a Hide test, shuffle a Nazgûl enemy from the discard pile
-- into the encounter deck, if able.
instance HasAbilities TheMarish where
  getAbilities (TheMarish a) = [mkAbility a 1 $ forced $ FailedHideTestWindow After AnyPlayer]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player
-- failed a Hide test this round).
instance RunMessage TheMarish where
  runMessage msg l@(TheMarish a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      pile <- getsGame ((.encounterDiscard) . toAttrs . (.scenario))
      let nazgul = filter (cardMatches $ CardWithTrait Nazgul <> CardWithType EnemyType) pile
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "theMarish.shuffle"
        causedBy a
        showing (map EncounterCard nazgul)
        for_ nazgul \c -> targeting (CardIdTarget c.id) do
          moveToTopOfEncounterDeck c
          shuffleEncounterDeck
      pure l
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      failed <- failedHideTestThisRound pid
      enemyGetsAttack (CardIdSource cid) eid (if failed then 2 else 1)
      pure l
    _ -> TheMarish <$> runMessage msg a
