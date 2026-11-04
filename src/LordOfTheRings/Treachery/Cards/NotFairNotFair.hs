module LordOfTheRings.Treachery.Cards.NotFairNotFair (notFairNotFair) where

import LordOfTheRings.Card (cdCardType, toCardDef)
import LordOfTheRings.Card.CardType (CardType (EnemyType, LocationType, ObjectiveType, TreacheryType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Message.Lifted.Choose
import LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (getRiddleStage)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NotFairNotFair = NotFairNotFair TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

notFairNotFair :: TreacheryCard NotFairNotFair
notFairNotFair = treachery NotFairNotFair Cards.notFairNotFair

-- | This effect cannot be canceled.
instance HasModifiersFor NotFairNotFair where
  getModifiersFor (NotFairNotFair a) = modifySelf a [WhenRevealedCannotBeCanceled]

{- | When Revealed: The first player names a card type and then discards the top 3 cards
of the encounter deck. For each of those cards that does not match the named type, remove
1 progress token from stage 2.

The types an encounter card can have: enemy, location, treachery or objective. Its text
removes the progress Riddles in the Dark otherwise keeps from all but riddles.
-}
instance RunMessage NotFairNotFair where
  runMessage msg t@(NotFairNotFair attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      top <- getsGame (take 3 . (.encounterDeck) . toAttrs . (.scenario))
      stage <- getRiddleStage
      chooseOneM lead do
        questionLabeled "notFairNotFair.name"
        causedBy attrs
        for_ [("enemy", EnemyType), ("location", LocationType), ("treachery", TreacheryType), ("objective", ObjectiveType)] \(k, ty) ->
          labeled ("notFairNotFair." <> k) do
            discardTopOfEncounterDeck attrs 3
            let misses = length $ filter ((/= ty) . cdCardType . toCardDef) top
            for_ stage \q -> do
              progress <- getsGame (maybe 0 (.progress) . lookup q . (.entities.quests))
              when (misses > 0 && progress > 0) $ removeTokens attrs q Progress (min misses progress)
      pure t
    _ -> NotFairNotFair <$> runMessage msg attrs
