module LordOfTheRings.Treachery.Cards.PredatoryWolves (predatoryWolves) where

import LordOfTheRings.Card (CardCost (StaticCost), cdCost, toCardDef)
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (searchToRevealWarg)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PredatoryWolves = PredatoryWolves TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

predatoryWolves :: TreacheryCard PredatoryWolves
predatoryWolves = treachery PredatoryWolves Cards.predatoryWolves

-- | When Revealed: Each player must choose: either discard the highest cost ally he
-- controls, or search the encounter deck and discard pile for a Warg enemy, reveal it, and
-- add it to the staging area. Shuffle the encounter deck.
-- Shadow: Exhaust a character you control.
-- Of allies tied for the highest cost, the player discards the one he chooses; one with no
-- printed cost counts as 0.
instance RunMessage PredatoryWolves where
  runMessage msg t@(PredatoryWolves attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        costs <- for allies \aid -> (aid,) . printedCost . toCardDef <$> getAlly aid
        let highest = maximumMay (map snd costs)
        chooseOrRunOneM p do
          questionLabeled "predatoryWolves.choose"
          causedBy attrs
          unless (null allies) $ labeled "predatoryWolves.discardAlly" do
            chooseOrRunOneM p do
              questionLabeled "predatoryWolves.chooseAlly"
              causedBy attrs
              targets [aid | (aid, c) <- costs, Just c == highest] discard
          labeled "predatoryWolves.search" $ searchToRevealWarg p attrs False
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust (CardIdSource cid) pid 1
      pure t
    _ -> PredatoryWolves <$> runMessage msg attrs
   where
    printedCost def = case cdCost def of
      Just (StaticCost x) -> x
      _ -> 0
