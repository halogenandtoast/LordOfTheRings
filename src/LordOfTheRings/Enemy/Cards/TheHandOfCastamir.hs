module LordOfTheRings.Enemy.Cards.TheHandOfCastamir (theHandOfCastamir) where

import LordOfTheRings.Card (Card (EncounterCard), EncounterCard, toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Entities (entitiesEnemies, entitiesLocations, entitiesTreacheries)
import LordOfTheRings.Game.Base (gameEntities)

newtype TheHandOfCastamir = TheHandOfCastamir EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHandOfCastamir :: EnemyCard TheHandOfCastamir
theHandOfCastamir = enemy TheHandOfCastamir Cards.theHandOfCastamir

-- | Forced: After The Hand of Castamir attacks, reveal the top card of the encounter deck,
-- resolve its effects, and discard it.
-- Forced: After a treachery card is revealed from the encounter deck, The Hand of Castamir
-- makes an immediate attack against the first player.
instance HasAbilities TheHandOfCastamir where
  getAbilities (TheHandOfCastamir a) =
    [ mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id
    , mkAbility a 2 $ forced $ TreacheryRevealedWindow After
    ]

instance RunMessage TheHandOfCastamir where
  runMessage msg e@(TheHandOfCastamir attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      -- an empty deck takes the discard pile shuffled in, as revealing would
      when (null deck) $ shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure e
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      deck <- getsGame ((.encounterDeck) . toAttrs . (.scenario))
      for_ (take 1 deck) \card -> do
        removeFromEncounterDeck card
        push $ RevealCard card
        doStep 2 (RevealCard card)
      pure e
    -- the card revealed, its effects resolved, is discarded from wherever it is
    DoStep 2 (RevealCard card) -> do
      discardRevealed attrs card
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    _ -> TheHandOfCastamir <$> runMessage msg attrs

discardRevealed :: (ReverseQueue m, Sourceable source) => source -> EncounterCard -> m ()
discardRevealed source card = do
  es <- getsGame gameEntities
  let it = EncounterCard card
  traverse_ (discard source)
    $ [toTarget x | x <- toList (entitiesEnemies es), toCard x == it]
    <> [toTarget x | x <- toList (entitiesLocations es), toCard x == it]
    <> [toTarget x | x <- toList (entitiesTreacheries es), toCard x == it]
